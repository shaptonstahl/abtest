<?php
//http://216.35.221.70/dashboard/cohortDump.php?startDate=2014-02-20&cohort=G

$testStart = urldecode($_GET["start"]);
if(isset($_GET["end"])) {
  $testEnd = urldecode($_GET["end"]);
} else {
  $testEnd = date('Y-m-d');
}
$cohort = $_GET["cohort"];

require('database-credentials-gracchus.php');

$sqlSyntax = <<<SQLSYNTAX
  SELECT ratings_cohort,
  SUM(ratings_elapsed) AS listen_time_seconds
  FROM infinite.user_ratings
  WHERE ratings_platform = "IPHONE"
    AND DATE(ratings_timestamp) >= "$testStart"
    AND DATE(ratings_timestamp) <= "$testEnd"
    AND ratings_rating IN ('SKIP','COMPLETED')
    AND ratings_cohort = "$cohort"
#    AND ratings_platform != "LOADTEST"
    AND ratings_user_id NOT IN (11987982, 13174734, 1186881532, 1087617437, 1138038188, 9906791, 7456245, 8943206, 12526723, 2019184, 100042, 11727645, 10853163, 11657281, 11420786) 
  GROUP BY ratings_user_id,
        DATE(ratings_timestamp);
SQLSYNTAX;

$result = mysql_query($sqlSyntax) or die(mysql_error());
while($row = mysql_fetch_array($result)){
  //    $duration = gmdate("H:i:s", $row["duration"]);
  echo $row["listen_time_seconds"] . "\n";
}
?>
