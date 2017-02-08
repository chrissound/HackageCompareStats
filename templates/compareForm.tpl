<!DOCTYPE html>
<html>
  <head>
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"></script>
    <script src="https://code.highcharts.com/highcharts.js"></script>
    <script src="https://code.highcharts.com/modules/exporting.js"></script>
    {{{ scriptInject }}}
  </head>
  <body>
    <form>
      <datalist id="packages">
        <packages>
          <option/>
        </packages>
      </datalist>
      <label for="package1">Package 1:</label>
      <input id="package1" name="package[]" type="text" list="packages">
      <br />
      <label for="package1">Package 2:</label>
      <input id="package2" name="package[]" type="text" list="packages">
      <br />
      <input type="submit" value="Compare" />
    </form>
    <statisticResult>
      <div id="container" style="min-width: 310px; max-width: 800px; height: 400px; margin: 0 auto">
      </div>
      <p>Requested packages: <requestedPackage/><br />
        Result: <statisticRemark/>
      </p>
    </statisticResult>
  </body>
</html>
