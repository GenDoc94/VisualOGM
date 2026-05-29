@echo off
magick -size 1280x640 gradient:#c5e3ff-#f5faff ^
  ( "%~dp0..\www\VO.png" -resize 220x ) -gravity west -geometry +100+0 -composite ^
  ( "%~dp0..\www\VisualOGM.png" -resize 620x ) -gravity center -geometry +60-20 -composite ^
  -gravity east -font Arial-Bold -pointsize 32 -fill "#004085" -annotate +80+120 "Circle plots and oncoprints" ^
  -font Arial -pointsize 26 -fill "#007BFF" -annotate +80+170 "Bionano OGM - Shiny" ^
  "%~dp0..\www\VisualOGM-social-1280x640.png"
