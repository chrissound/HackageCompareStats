<!DOCTYPE html>
<html>
  <body>
    <h1>Arch Linux Package Statistic Comparison</h1>
    <form action="/comparePackage">
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
  </body>
</html>