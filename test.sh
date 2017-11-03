fn $factorial($n) {
    $fact = $n;
    while $n != 1 {
        $n = $n - 1;
        $fact = $fact * $n;
    }
    return $fact;
}

$echo($factorial($int($arg[0])));