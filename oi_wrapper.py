import os
import sys
import socket
import logging
from optparse import OptionParser

import offlineimap
from offlineimap import globals as glob
from offlineimap.CustomConfig import CustomConfigParser
from offlineimap.ui import UI_LIST, setglobalui
from offlineimap import threadutil, accounts
from offlineimap.folder.IMAP import MSGCOPY_NAMESPACE


ACCOUNT_LIMITED_THREAD_NAME = 'MAX_ACCOUNTS'


class OfflineImapWrapper(offlineimap.OfflineImap):
    def __init__(self, config_str="", accounts=None, folders=None,
                 quick=False, logfile=None):
        super().__init__()
        self.__config_str = config_str
        self.__accounts = accounts
        self.__folders = folders
        self.__quick = quick
        self.__logfile = logfile

    # TODO: Rename this and separate options parsing from config and args generation
    # NOTE: overrriding "private" method
    def _OfflineImap__parse_cmd_options(self):
        parser = OptionParser(
            version=offlineimap.__version__,
            description="%s.\n\n%s" % (offlineimap.__copyright__,
                                       offlineimap.__license__)
        )

        parser.add_option("-V",
                          action="store_true", dest="version",
                          default=False,
                          help="show full version infos")

        parser.add_option("--dry-run",
                          action="store_true", dest="dryrun",
                          default=False,
                          help="dry run mode")

        parser.add_option("--info",
                          action="store_true", dest="diagnostics",
                          default=False,
                          help="output information on the configured email repositories")

        parser.add_option("-1",
                          action="store_true", dest="singlethreading",
                          default=False,
                          help="(the number one) disable all multithreading operations")

        parser.add_option("-P", dest="profiledir", metavar="DIR",
                          help="sets OfflineIMAP into profile mode.")

        parser.add_option("-a", dest="accounts",
                          metavar="account1[,account2[,...]]",
                          help="list of accounts to sync")

        parser.add_option("-c", dest="configfile", metavar="FILE",
                          default=None,
                          help="specifies a configuration file to use")

        parser.add_option("-d", dest="debugtype",
                          metavar="type1[,type2[,...]]",
                          help="enables debugging for OfflineIMAP "
                               " (types: imap, maildir, thread)")

        parser.add_option("-l", dest="logfile", metavar="FILE",
                          help="log to FILE")

        parser.add_option("-s",
                          action="store_true", dest="syslog",
                          default=False,
                          help="log to syslog")

        parser.add_option("-f", dest="folders",
                          metavar="folder1[,folder2[,...]]",
                          help="only sync the specified folders")

        parser.add_option("-k", dest="configoverride",
                          action="append",
                          metavar="[section:]option=value",
                          help="override configuration file option")

        parser.add_option("-o",
                          action="store_true", dest="runonce",
                          default=False,
                          help="run only once (ignore autorefresh)")

        parser.add_option("-q",
                          action="store_true", dest="quick",
                          default=False,
                          help="run only quick synchronizations (don't update flags)")

        parser.add_option("-u", dest="interface",
                          help="specifies an alternative user interface"
                               " (quiet, basic, syslog, ttyui, blinkenlights, machineui)")

        parser.add_option("--delete-folder", dest="deletefolder",
                          default=None,
                          metavar="FOLDERNAME",
                          help="Delete a folder (on the remote repository)")

        parser.add_option("--migrate-fmd5-using-nametrans",
                          action="store_true", dest="migrate_fmd5", default=False,
                          help="migrate FMD5 hashes from versions prior to 6.3.5")

        parser.add_option("--mbnames-prune",
                          action="store_true", dest="mbnames_prune", default=False,
                          help="remove mbnames entries for accounts not in accounts")

        (options, args) = parser.parse_args()
        glob.set_options(options)

        if options.version:
            print(("offlineimap v%s, %s" % (
                offlineimap.__version__, self.get_env_info())
                   ))
            sys.exit(0)

        # Read in configuration file.
        if not options.configfile:
            # Try XDG location, then fall back to ~/.offlineimaprc
            xdg_var = 'XDG_CONFIG_HOME'
            if xdg_var not in os.environ or not os.environ[xdg_var]:
                xdg_home = os.path.expanduser('~/.config')
            else:
                xdg_home = os.environ[xdg_var]
            options.configfile = os.path.join(xdg_home, "offlineimap", "config")
            if not os.path.exists(options.configfile):
                options.configfile = os.path.expanduser('~/.offlineimaprc')
            configfilename = options.configfile
        else:
            configfilename = os.path.expanduser(options.configfile)

        config = CustomConfigParser()
        # NOTE: Read config string if exists
        if self.__config_str:
            config.read_string(self.__config_str)
        else:
            if not os.path.exists(configfilename):
                # TODO, initialize and make use of chosen ui for logging
                logging.error(" *** Config file '%s' does not exist; aborting!" %
                              configfilename)
                sys.exit(1)
            config.read(configfilename)
        options.accounts = self.__accounts
        options.folders = self.__folders
        options.quick = self.__quick
        options.logfile = self.__logfile

        # Profile mode chosen?
        if options.profiledir:
            if not options.singlethreading:
                # TODO, make use of chosen ui for logging
                logging.warning("Profile mode: Forcing to singlethreaded.")
                options.singlethreading = True
            if os.path.exists(options.profiledir):
                # TODO, make use of chosen ui for logging
                logging.warning("Profile mode: Directory '%s' already exists!" %
                                options.profiledir)
            else:
                os.mkdir(options.profiledir)
            # TODO, make use of chosen ui for logging
            logging.warning("Profile mode: Potentially large data will be "
                            "created in '%s'" % options.profiledir)

        # Override a config value.
        if options.configoverride:
            for option in options.configoverride:
                (key, value) = option.split('=', 1)
                if ':' in key:
                    (secname, key) = key.split(':', 1)
                    section = secname.replace("_", " ")
                else:
                    section = "general"
                config.set(section, key, value)

        # Which ui to use? CLI option overrides config file.
        ui_type = config.getdefault('general', 'ui', 'ttyui')
        if options.interface is not None:
            ui_type = options.interface
        if '.' in ui_type:
            # Transform Curses.Blinkenlights -> Blinkenlights.
            ui_type = ui_type.split('.')[-1]
            # TODO, make use of chosen ui for logging
            logging.warning('Using old interface name, consider using one '
                            'of %s' % ', '.join(list(UI_LIST.keys())))
        if options.diagnostics:
            ui_type = 'ttyui'  # Enforce this UI for --info.

        # dry-run? Set [general]dry-run=True.
        if options.dryrun:
            config.set('general', 'dry-run', 'True')
        config.set_if_not_exists('general', 'dry-run', 'False')

        try:
            # Create the ui class.
            self.ui = UI_LIST[ui_type.lower()](config)
        except KeyError:
            logging.error("UI '%s' does not exist, choose one of: %s" %
                          (ui_type, ', '.join(list(UI_LIST.keys()))))
            sys.exit(1)
        setglobalui(self.ui)

        # Set up additional log files.
        if options.logfile:
            self.ui.setlogfile(options.logfile)

        # Set up syslog.
        if options.syslog:
            self.ui.setup_sysloghandler()

        # Welcome blurb.
        self.ui.init_banner()
        self.ui.info(self.get_env_info())

        if options.debugtype:
            self.ui.logger.setLevel(logging.DEBUG)
            if options.debugtype.lower() == 'all':
                options.debugtype = 'imap,maildir,thread'
            # Force single threading?
            if not ('thread' in options.debugtype.split(',')
                    and not options.singlethreading):
                self.ui._msg("Debug mode: Forcing to singlethreaded.")
                options.singlethreading = True

            debugtypes = options.debugtype.split(',') + ['']
            for dtype in debugtypes:
                dtype = dtype.strip()
                self.ui.add_debug(dtype)

        if options.runonce:
            # Must kill the possible default option.
            if config.has_option('DEFAULT', 'autorefresh'):
                config.remove_option('DEFAULT', 'autorefresh')
            # FIXME: spaghetti code alert!
            for section in accounts.getaccountlist(config):
                config.remove_option('Account ' + section, "autorefresh")

        if options.quick:
            for section in accounts.getaccountlist(config):
                config.set('Account ' + section, "quick", '-1')

        # Custom folder list specified?
        if options.folders:
            foldernames = options.folders.split(",")
            folderfilter = "lambda f: f in %s" % foldernames
            folderincludes = "[]"
            for accountname in accounts.getaccountlist(config):
                account_section = 'Account ' + accountname
                remote_repo_section = 'Repository ' + \
                                      config.get(account_section, 'remoterepository')
                config.set(remote_repo_section, "folderfilter", folderfilter)
                config.set(remote_repo_section, "folderincludes",
                           folderincludes)

        if options.logfile:
            sys.stderr = self.ui.logfile

        socktimeout = config.getdefaultint("general", "socktimeout", 0)
        if socktimeout > 0:
            socket.setdefaulttimeout(socktimeout)

        threadutil.initInstanceLimit(
            ACCOUNT_LIMITED_THREAD_NAME,
            config.getdefaultint('general', 'maxsyncaccounts', 1)
        )

        for reposname in config.getsectionlist('Repository'):
            # Limit the number of threads. Limitation on usage is handled at the
            # imapserver level.
            for namespace in [accounts.FOLDER_NAMESPACE + reposname,
                              MSGCOPY_NAMESPACE + reposname]:
                if options.singlethreading:
                    threadutil.initInstanceLimit(namespace, 1)
                else:
                    threadutil.initInstanceLimit(
                        namespace,
                        config.getdefaultint(
                            'Repository ' + reposname,
                            'maxconnections', 2)
                    )
        self.config = config
        return options, args


if __name__ == '__main__':
    oi = OfflineImapWrapper()
    oi.run()
