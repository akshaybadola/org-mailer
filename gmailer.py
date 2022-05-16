from typing import List, Callable, Dict, Optional
import os
import json
from pathlib import Path
import shlex
import argparse
from subprocess import Popen, PIPE
import base64

# import configargparse
from flask import Flask, request
from werkzeug import serving

from oauth2client.client import OAuth2Credentials
from googleapiclient import discovery


def encode(v):
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
        credentials_file: File from which to read credentials.

    """
    def __init__(self, port: int, users: List[str], method: str,
                 credentials_file: Optional[Path]):
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
        self.app = Flask("gmail server")

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

    def read_creds_pass(self, user: str):
        """Read credentials from UNIX :code:`pass` command.

        Args:
            user: The user for whom to read credentials

        The credentials should be stored as :code:`mail/username` in the pass
        database.

        """
        try:
            p = Popen(shlex.split(f"pass mail/{user}"), stdout=PIPE)
            out, err = p.communicate()
            return OAuth2Credentials.from_json(out)
        except Exception:
            print(f"Could not read credentials for user {user}")
            return None

    def read_creds_plain(self, user: str):
        """Read credentials from plain text JSON.

        Args:
            user: The user for whom to read credentials

        Plain text JSON credentials *SHOULD NOT BE USED* except for debugging as
        it is a security risk.

        """
        try:
            with open(self.credentials_file) as f:  # type: ignore
                credentials = json.load(f)
                return OAuth2Credentials.from_json(credentials[user])
        except Exception:
            print(f"Could not read credentials for user {user}")
            return None

    def send_message(self, user: str, message: str):
        """Send mail via account of user

        Args:
            user: username  of gmail account
            message: The mail message
        """
        return self.service[user].users().\
            messages().send(userId="me", body={"raw": encode(message)}).execute()

    def run(self):
        """Start the :class:`Flask` service
        """
        @self.app.route("/sendmail", methods=["GET"])
        def __sendmail():
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
    args = parser.parse_args()
    port = args.port
    users = [x.strip() for x in args.users.split(",")]
    mailer = Mailer(port, users, args.method, Path(args.credentials_file))
    mailer.run()
