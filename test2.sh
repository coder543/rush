$val = $arg[0];
$panic($PATH);
for $path in $PATH {
    $echo("path: " + $path);
}
$echo(sandbox.exe $val # );