(setq tex-dvi-view-command "/usr/bin/xdvi \"*\"")
;(setq tex-dvi-view-command "set -xv; for i in \"*\"; do cd `/home/dsf/apt-source/shellutils-2.0.11/src/dirname \"$i\"`; xdvi `basename \"$i\"`; done")
(setq tex-dvi-print-command "dvips -t letter -D 600 -R -f < * | kprint")

