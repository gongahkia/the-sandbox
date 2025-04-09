<?php

// ----- imports -----

spl_autoload_register(
    function($class){
        require_once "class/$class.php";
    }
);

// ----- presets -----

?>

<html>
    <body>
        <h1>ftl</h1>

        <table border='1'>

<?php

// FUA
    // just use this as the equivalent of display.php
    // add display code here

// ----- execution body -----

echo "pee pee poo poo"

?>

        </table>
    </body>
</html>