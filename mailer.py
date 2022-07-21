from typing import List, Callable, Dict, Optional
import os
import json
import datetime
from pathlib import Path
import time
import shlex
import argparse
from subprocess import Popen, PIPE
import base64
import sys
import multiprocessing as mp
from threading import Thread
from functools import partial

from flask import Flask, request
from werkzeug import serving

# NOTE: I've incorporated only gmail but any other mail service
#       that supports IMAP and a send function via POP or RPC
#       mechanism should work fine
from google.oauth2.credentials import Credentials
from googleapiclient import discovery

import schedule

from oi_wrapper import OfflineImapWrapper

__version__ = "0.1.0"


def run_oi(config_str, **kwargs):
    """Run the OfflineIMAP.

    Args:
        config_str: configuration string as it would be in `.offlineimaprc` or
                    the offlineimap configuration
        kwargs: Keyword arguments to the :class:`OfflineImapWrapper`

    The wrapper :class:`OfflineImapWrapper` overrides :class:`OfflineImap`
    to incorporate running without a config file present.


    """
    print(f"Fetching mail at {datetime.datetime.now().ctime()}")
    oi = OfflineImapWrapper(config_str=config_str, **kwargs)
    oi.run()


def encode(v: str) -> str:
    """Encode string `v` to base64 string

    Args:
        v: String to be encoded

    Encode first to bytes, then to base64, then decode again. Perhaps all this
    is to remove artefacts. Not really sure but it works.

    """
    byte_msg = v.encode(encoding="UTF-8")
    byte_msg_b64encoded = base64.b64encode(byte_msg)
    return byte_msg_b64encoded.decode(encoding="UTF-8")


class Mailer:
    """Send mail via Gmail running as a :class:`Flask` service.

    A simple HTTP server that reads OAUTH credentials once and stores them
    in memory, and can then send mails via multiple accounts as requested.

    The mail must be properly encoded.

    Args:
        port: Port on which to bind the :class:`Flask` server
        users: List of users
        method: Method of reading the credentials
                Can be one of `pass` or `plain`. Additional methods can be added easily.
        schedule_mail: Whether to schedule fetching of mail at regular intervals
        schedule_times: If mail is scheduled then at what times should we do it.
        credentials_file: File from which to read credentials.

    """
    def __init__(self, port: int, users: List[str], method: str,
                 schedule_mail: bool, schedule_times: dict,
                 credentials_file: Optional[Path]):
        mp.set_start_method("spawn")
        self.port = port
        self.service = {}
        self.creds = {}
        self.users = users
        self.credentials_methods: Dict[str, Callable] = {"pass": self.read_creds_pass,
                                                         "plain": self.read_creds_plain}
        self.method = method
        self.credentials_file = credentials_file
        print(f"Read credentials method is: {self.method}")
        if not users:
            raise AttributeError("Has to be at least one user")
        print(f"Running for users: {self.users}")
        for user in self.users:
            self.creds[user] = self.read_creds(user)
            if self.creds[user] is not None:
                self.service[user] = discovery.build('gmail', 'v1', credentials=self.creds[user])
            else:
                self.service[user] = None
        self.offlineimaprc = self.read_offlineimap_config()
        self.oi_default_logfile = Path.home().joinpath("logs", "offlineimap.log")
        self.app = Flask("gmail server")
        if schedule_mail:
            self.schedule_times = schedule_times
            self.schedule_fetch_mail()

    def fetch_mail(self, quick=False, **kwargs):
        """Fetch mail according to given configuration

        Args:
            quick: Use \"quick\" option of offlineimap

        The users and other other options are specified via kwargs

        """
        kwargs = {"config_str": self.offlineimaprc,
                  "quick": quick}
        default_logfile = self.oi_default_logfile
        if default_logfile.exists():
            kwargs["logfile"] = str(default_logfile)
        oi = mp.Process(target=run_oi, kwargs=kwargs)
        oi.start()

    def schedule_fetch_mail(self):
        """Schedule periodic fetching of user mail

        We fetch once every day with full flags and \"quick\" every 15 minutes
        by default. The schedule can be configured via command line args
        \"--schedule-daily\" and \"--schedule-quick\".

        """
        def schedule_run_forever():
            while True:
                schedule.run_pending()
                time.sleep(60)

        t_time = self.schedule_times["daily"]
        t_mins = self.schedule_times["mins"]
        fetch_mail_quick = partial(self.fetch_mail, True)
        schedule.every().day.at(t_time).do(self.fetch_mail)
        schedule.every(t_mins).minutes.do(fetch_mail_quick)
        self.schedule_thread = Thread(target=schedule_run_forever)
        self.schedule_thread.start()
        print("Scheduled mail fetching service\n" +
              f"Full at every day at {t_time}\n" +
              f"Quick at every {t_mins} minutes")

    @property
    def method(self) -> str:
        """Get current credentials method."""
        return self._method

    @method.setter
    def method(self, m):
        if m in self.credentials_methods:
            self._method = m
        else:
            raise AttributeError(f"Unknown read credentials method {m}")

    @property
    def read_creds(self) -> Callable:
        """Read credentials"""
        return self.credentials_methods[self.method]

    def read_creds_pass(self, user: str) -> Optional[Credentials]:
        """Read credentials from UNIX :code:`pass` command.

        Args:
            user: The user for whom to read credentials

        The credentials should be stored as :code:`mail/username` in the pass
        database.

        """
        try:
            p = Popen(shlex.split(f"pass mail/{user}"), stdout=PIPE)
            out, err = p.communicate()
            return Credentials.from_authorized_user_info(json.loads(out))
        except Exception:
            print(f"Could not read credentials for user {user}")
            return None

    def read_creds_plain(self, user: str) -> Optional[Credentials]:
        """Read credentials from plain text JSON.

        Args:
            user: The user for whom to read credentials

        Plain text JSON credentials *SHOULD NOT BE USED* except for debugging as
        it is a security risk.

        """
        try:
            with open(self.credentials_file) as f:  # type: ignore
                credentials = json.load(f)
                return Credentials.from_authorized_user_info(credentials)
        except Exception:
            print(f"Could not read credentials for user {user}")
            return None

    def read_offlineimap_config(self) -> Optional[str]:
        """Read offlineimap configuration from password store.

        The path in the password store is assumed to be :code:`mail/offlineimaprc`

        """
        try:
            p = Popen(shlex.split(f"pass mail/offlineimaprc"), stdout=PIPE)
            out, err = p.communicate()
            return out.decode()
        except Exception:
            print(f"Could not read offlineimap config")
            return None

    def send_message(self, user: str, message: str):
        """Send mail via account of user

        Args:
            user: username  of gmail account
            message: The mail message
        """
        return self.service[user].users().\
            messages().\
            send(userId="me", body={"raw": encode(message)}).\
            execute()

    def run(self):
        """Start the :class:`Flask` service
        """
        @self.app.route("/version", methods=["GET"])
        def __version():
            return __version__

        @self.app.route("/sendmail", methods=["GET"])
        def __sendmail():
            """Send mail via Google's api

            Args:
                user: User name or account
                filename: Mail file

            """
            try:
                user = request.args.get("user")
                filename = request.args.get("filename")
            except Exception:
                return "Error. No user or filename in data"
            with open(os.path.join(os.path.expanduser("~/" + filename))) as f:
                msg = f.read()
            if user not in self.users:
                return f"Error. Unknown User {user}"
            else:
                return "Success. " + json.dumps(self.send_message(user, msg))

        @self.app.route("/fetch_mail", methods=["GET"])
        def __offllineimap():
            """Fetch mail via offlineimap

            Args:
                accounts: comma separated list of accounts
                folders: comma separated list of folders to sync
                quick: quick flag to OfflineImap
                logfile: specify logfile for OfflineImap

            """
            kwargs = {"config_str": self.offlineimaprc}
            for x in ["accounts", "folders", "quick"]:
                if request.args.get(x):
                    kwargs[x] = request.args.get(x)
            default_logfile = self.oi_default_logfile
            if default_logfile.exists():
                kwargs["logfile"] = str(request.args.get("logfile") or default_logfile)
            oi = mp.Process(target=run_oi, kwargs=kwargs)
            oi.start()
            accounts = kwargs.get("accounts", "all")
            folders = kwargs.get("folders", "all")
            quick = kwargs.get("quick", False)
            return (f"Fetching for accounts: {accounts} and folders: {folders}\n" +
                    f"Options: quick {quick}, logfile {kwargs.get('logfile', None)}")
        serving.run_simple("localhost", self.port, self.app, threaded=True)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("--users", "-u", type=str,
                        default="",
                        help="Comma separated user names")
    parser.add_argument("--port", "-p", type=int, default=1234,
                        help="port on which to serve")
    parser.add_argument("--method", "-m", default="pass",
                        help="Method for reading credentials. Defaults to 'pass'")
    parser.add_argument("--credentials-file", default="",
                        help="Plain text credentials file. NOT RECOMMENDED. Can be used for debugging.")
    parser.add_argument("--schedule", action="store_true",
                        help="Use scheduler for fetching of user mail.")
    parser.add_argument("--schedule-daily", default="05:00",
                        help="Schedule fetching all mail at a specified time every day.")
    parser.add_argument("--schedule-mins", default=15, type=int,
                        help="Schedule fetching mail with \"quick\" flag in every \"x\" minutes.")
    args = parser.parse_args()
    sys.argv = sys.argv[:1]  # reset args to avoid confusing OI and keep changes to a minimum
    port = args.port
    users = [x.strip() for x in args.users.split(",")]
    mailer = Mailer(port, users, args.method, args.schedule,
                    {"daily": args.schedule_daily, "mins": args.schedule_mins},
                    Path(args.credentials_file))
    mailer.run()
