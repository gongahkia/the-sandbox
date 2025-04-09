<?php
// ---------- SESSION MANAGAEMENT ----------
    // persists saves across mutliple sessions without need for local files

session_start(); // starts or resumes session

if (!isset($_SESSION['coordArray'])) {
    $_SESSION['coordArray'] = [0, 0]; // initializes int array if unset in session
}

// ---------- USER INPUT ----------

$coordArray = $_SESSION['coordArray'];

if ($_SERVER["REQUEST_METHOD"] == "POST") {
    if (isset($_POST["button_w"])) {
        $coordArray[1] -= 1;
    } elseif (isset($_POST["button_a"])) {
        $coordArray[0] -= 1;
    } elseif (isset($_POST["button_s"])) {
        $coordArray[1] += 1;
    } elseif (isset($_POST["button_d"])) {
        $coordArray[0] += 1;
    } else {
        // do something
    }

    $_SESSION['coordArray'] = $coordArray; // updates the local session variable
    var_dump($coordArray);
}
?>

<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>tikrit</title>
</head>
<body>

<form method="post" action="<?php echo htmlspecialchars($_SERVER["PHP_SELF"]); ?>">
    <button type="submit" name="button_w">W</button>
    <button type="submit" name="button_a">A</button>
    <button type="submit" name="button_s">S</button>
    <button type="submit" name="button_d">D</button>
</form>

</body>
</html>
