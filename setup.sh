
EDOT="$HOME/.emacs.d"
CWD=$(pwd)
echo $CWD

mv "$EDOT/init.el" "$EDOT/init.el.backup"
mv "$EDOT/early-init.el" "$EDOT/early-init.el.backup"
mv "$EDOT/emacs-dx" "$EDOT/emacs-dx.backup"

ln -s "$CWD/emacs.d/init.el" "$EDOT/init.el"
ln -s "$CWD/emacs.d/early-init.el" "$EDOT/early-init.el"
ln -s "$CWD/emacs.d/emacs-dx" "$EDOT/emacs-dx"

ls -lta $EDOT
