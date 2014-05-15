<?php
//http://172.31.2.98//ab-cohorts.php

require('database-credentials-gracchus.php');

$sqlSyntax = <<<SQLSYNTAX
  SELECT DISTINCT ratings_cohort
  FROM infinite.user_ratings
  WHERE ratings_platform = "IPHONE"
    AND ratings_rating IN ('SKIP','COMPLETED')
    AND ratings_platform != "LOADTEST"
    AND ratings_user_id NOT IN (11987982, 13174734, 1186881532, 1087617437, 1138038188, 9906791, 7456245, 8943206, 12526723, 2019184, 100042, 11727645, 10853163, 11657281, 11420786)
SQLSYNTAX;

$result = mysql_query($sqlSyntax) or die(mysql_error());
while($row = mysql_fetch_array($result)){
  //    $duration = gmdate("H:i:s", $row["duration"]);
  echo $row["ratings_cohort"] . "\n";
}
?>
