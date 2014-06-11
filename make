if cabal install ; then
  echo "[Start]"
  time RayMarch
  echo "[Done]"
  gqview out.png
fi