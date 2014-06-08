if cabal install ; then
  echo "[Start]"
  RayMarch
  echo "[Done]"
  gqview out.png
fi