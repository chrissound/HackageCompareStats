<!DOCTYPE html>
<html>
  <head>
    <script>
      (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
      (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
      m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
      })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

      ga('create', 'UA-92879541-1', 'auto');
      ga('send', 'pageview');

    </script>
    <baseUrl/>
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"></script>
    <script src="https://code.highcharts.com/highcharts.js"></script>
    <script src="https://code.highcharts.com/modules/exporting.js"></script>
    <script>
    {{{ scriptInject }}}
    </script>
    <title><requestedPackagesVs/> - Arch Linux Package Statistic Comparison</title>
  </head>
  <body>
    <h1><requestedPackagesVs/></h1>
    <h2>Arch Linux Package Statistic Comparison</h2>
    <h3>Featured:</h3>
    <ul>
      <li><a href="comparePackage/gnome-terminal/lxterminal/rxvt/rxvt-unicode/st/terminator/termite/xterm">Terminals</a></li>
      <li><a href="comparePackage/atom/emacs/nano/neovim/sublime-text/vim">Editors</a></li>
      <li><a href="comparePackage/cower/pacaur/packer/yaourt">AUR helpers</a></li>
      <li><a href="comparePackage/chromium/firefox/google-chrome/links/lynx/midori/opera">Internet browsers</a></li>
      <li><a href="comparePackage/awesome/cinnamon/i3-wm/lxde-common/mate-desktop/mutter/openbox/plasma-desktop/xfdesktop/xmonad">Desktop environments / Windows managers</a></li>
    </ul>
    <h3>Custom selection:</h3>
    <form action="comparePackage">
      <datalist id="packages">
        <packages/>
      </datalist>
      <label for="package1">Package 1:</label>
      <input id="package1" name="package[]" type="text" list="packages">
      <br />
      <label for="package2">Package 2:</label>
      <input id="package2" name="package[]" type="text" list="packages">
      <br />
      <label for="package3">Package 3:</label>
      <input id="package3" name="package[]" type="text" list="packages">
      <br />
      <label for="package4">Package 4:</label>
      <input id="package4" name="package[]" type="text" list="packages">
      <br />
      <label for="package5">Package 5:</label>
      <input id="package5" name="package[]" type="text" list="packages">
      <br />
      <input type="submit" value="Compare" />
    </form>
    <statisticResult>
      <div id="container" style="min-width: 310px; max-width: 800px; height: 400px; margin: 0 auto">
      </div>
    </statisticResult>
  </body>
</html>
