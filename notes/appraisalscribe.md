# Undeleting reports

1. Find out which uuids need to be undeleted
2. appraisalscope --state /srv/appraisalscribe-production/_state/appraisalData undelete <uuid>

# Parsing event files

# Splitting and inserting new pages - given report.pdf and pages.pdf:

pdfseparate report.pdf report-%d.pdf
Rename pages 1 thru 9 with leading zeros
Split the new pages
See what pages they represent, and move them to replace the old pages
pdfjoin report-??.pdf
Rename report-44-joined.pdf -> report-joined.pdf

# Deploying on nix

Log into gene as dsf
Run, su nixops, and enter the default user password
cd ~/nix-seereason
Run git status and make sure all changes are committed and pushed
Run get pull to make sure nix-seereason is up to date
Cd ~/nix-seereason/pkgs
Run ./update.sh (will check hackage, gitup etc for newer version of packages)
Run git status and see what packages were updated
Run nixops deploy -d appraisalscribe-ec2 (that should rebuild everything, push it to ﻿⁠⁠⁠⁠alpha.apraisalscribe.com﻿⁠⁠⁠⁠ and activate the new configuration)
Run git add -u; git commit -m ‘Some commit message’; git push

if you want to update some, but not all of the packages, then you can copy and paste specific lines from ﻿⁠⁠⁠⁠./update.sh﻿⁠⁠⁠⁠ and run them on the command line. However, that requires things like ﻿⁠⁠⁠⁠cabal2nix﻿⁠⁠⁠⁠ to be in your search path. The way to do that is to run ﻿⁠⁠⁠⁠nix-shell﻿⁠⁠⁠⁠ in the ﻿⁠⁠⁠⁠~/nix-seereason/pkgs﻿⁠⁠⁠⁠ directory. There is a ﻿⁠⁠⁠⁠shell.nix﻿⁠⁠⁠⁠ file that ﻿⁠⁠⁠⁠nix-shell﻿⁠⁠⁠⁠ will use to get you the correct configuration.

You don’t have to do that when you use ﻿⁠⁠⁠⁠./update.sh﻿⁠⁠⁠⁠ because it starts with
```#! /usr/bin/env nix-shell
#! nix-shell -i sh -p cabal2nix cabal-install
```

so, it runs ﻿⁠⁠⁠⁠nix-shell﻿⁠⁠⁠⁠ for you

david [9:42 AM] - can you build without deploying?
stepcut [9:47 AM] ￼ - --build-only
david [9:48 AM] ￼- and when you are ready to deploy?
stepcut [9:49 AM]  - Drop the flag

# Error message:

$ nixops deploy -d appraisalscribe-ec2
error: state file ‘/home/jeremy/.nixops’ should have extension ‘.nixops’
(This was caused by a bad line in my .bashrc)



# Stop and start the server

systemctl restart appraisalscribe
systemctl stop appraisalscribe
systemctl start appraisalscribe

# Add a new hackage package

Add a new line to update.sh: (cd show-please && cabal2nix cabal://show-please > default.nix) - keep file sorted
Create show-please subdir
Run update.sh
git add show-please/default.nix
Add a line to seereason-ghc802.nix:      "show-please" = self.callPackage ./pkgs/show-please {};
Build and commit, or commit and build.

...

stepcut [1:33 PM] ￼
some of the other entries in that file have extra options such as disabling the test suite or using fetchgitPrivate. But in this case we should not need any special options

...

that procedure is specifically for haskell packages. I had to do something also the same for fonts-arphic-ukai

except I manually created the fonts-arphic-ukai/default.nix and I added it to seereason-overrides.nix

same for the extra javascript files like json2 and jstree

./update.sh currently makes no attempts to update those, but we probably don’t need them updated anyway — in fact, I already had to downgrade some of the javascript packages to get them to work with appraisalscribe :)


Discover what version of pandoc-types we are building

Look in /nix/store on alpha.seereason.com
