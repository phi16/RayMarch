if cabal install ; then
  echo "[Start]"
  callMiyu "info Start Rendering" "info Finish Rendering" time RayMarch
  echo "[Done]"
  va=`date +"%Y%m%d%H%M"`
  cp out.png ~/Dropbox/folds/$va.png
  gqview out.png
fi