<!-- FUA -->
    <!-- add JS to handle clearing the entire body, showing game over screen when the body is empty -->
    <!-- add logic to randomise questions and group together from a nested array with tags of questiontype that takes enum as a value and actual question and answer content -->
    <!-- add a bunch of default displayed achievements similar to lethal company after the round is over, including longest response, shortest response, response with the most a's etc, fastest time, best answer, worst answer -->
    <!-- randomise question types, questions and input types -->
        <!-- you need to type the given randomised text as fast as possible -->
        <!-- checkbox where u check all the even / odd / prime / factor of / multiple of / square root / factorial numbers / fiboonacci -->
    <!-- can i integrate a table into this question?? -->
    <!-- work out the specified questions to use and the specified text type to use for each kind of form -->
    <!-- add sprites from itch io for characters and make the guy sway around frantically -->
    <!-- add proper css styling and html styling -->
    <!-- have some form of input validaton method if thats even needed -->
    <!-- track state and have a local highscore implemented -->
    <!-- playtest extensively -->

<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>registration closes in 1 minute</title>
</head>
<body>
    <script>
        var countDown = new Date().getTime() + 60 * 1000;
        var countdownInterval = setInterval(function() {
            var currentTime = new Date().getTime();
            var remaining = countDown- currentTime;
            var secondsRemaining = Math.floor((remaining % (1000 * 60)) / 1000);
            document.getElementById("timer").innerHTML = secondsRemaining + " seconds left";
            if (remaining < 0) {
                clearInterval(countdownInterval);
                // document.getElementById("timer").innerHTML = "TIME'S UP!";
                document.querySelector("body").innerHTMl = "";
                document.querySelector("body").innerHTMl = "TIME'S UP!";
            }
        }, 1000);
    </script>
    <center>
        <u><p id="timer"></p></u>
    </center>
    <?php

        // ---------- FUNCTION PRESETS -----------

        enum inputType {
            case text;
            case password;
            case number;
            case email;
            case file;
            case date;
            case range;
            case time;
            case color;
            case radio;
            case option;
        }

        // ---------- FUNCTION DEFINITION ----------

        function createHeader(): String {
            $headerArray = array(
                "Minister of Parliament Signups",
                "Speaker of Parliament Signups",
                "Presidential Election Signups",
                "NUS Student Application",
                "NTU Student Application",
                "SUSS Student Application",
                "SIM Student Application",
                "Squidward Community College Application",
                "Sam Sulek Community College Application",
                "SMU Gravical Signups",
                "Monster Hunter College Signups",
                "Jujutsu Technical School Signups",
                "Isle Eating House Server Signups",
                "Isle Eating House Chef Signups",
                "Isle Eating House Cashier Signups",
                "Isle Eating House Drinks Maker Signups",
                "Isle Eating House Upper Management Signups",
                "A9 Dumplings Upper Management Signups",
                "Mr Olympia Signups",
                "Chief Justice Signups",
                "Chief Clown Registration",
                "Chief Idiot Registration",
                "King Kong Curry Signups",
                "McDonald Tryouts Signups",
                "Legal Systems TA Signups",
                "Contract Law 1 TA Signups",
                "Contract Law 2 TA Signups",
                "Criminal Law Signups",
                "Petition to Go Home",
                "Petition to Drop Mod",
                "Petition to Dropout",
                "Petition to Cry",
                "Petition to be a Fart Smella",
                "Petition to be a Smart Fella",
                "Aniplus employee Registration",
                "Koufu employee Registration",
                "Gongcha employee Registration",
                "Uniqlo employee Registration",
                "HnM employee Registration",
                "Chinese Tofu Magician employee Registration",
                "Go Sweep Road Registration",
                "Go Fly Kite Registration",
                "Fighting Demons Registration",
                "Koi employee Registration"
            );
            $fin = $headerArray[rand(0,count($headerArray)-1)];
            return "<h1>$fin</h1>";
        }

        function createSubmitInput(String $value="SUBMIT HERE!!!"): String {
            return "<input type='submit' value='$value'>";
        }

        function createSingleInput(inputType $in, String $text, Int $count): String {
            $type = "";
            switch ($in) {
                case inputType::text:
                    $type = "text";
                    break;
                case inputType::password:
                    $type = "password";
                    break;
                case inputType::number:
                    $type = "number";
                    break;
                case inputType::email:
                    $type = "email";
                    break;
                case inputType::file:
                    $type = "file";
                    break;
                case inputType::date:
                    $type = "date";
                    break;
                case inputType::range:
                    $type = "range";
                    break;
                case inputType::time:
                    $type = "time";
                    break;
                case inputType::color:
                    $type = "color";
                    break;
                default:
                    $type = "ERROR 01"; // this case should never actually hit since we're using rust-like enums for state modelling
            }
            return "<label>$text<input type='$type' name='$count'></input><br></label>";
        }

        function createMultipleInput(inputType $in, array $text, String $name = "filler"): String {
            $type = "";
            $fin = "";

            switch ($in) {
                case inputType::radio: // radio must have same name but id can be different to allow for only selection of one input
                    $type = "radio";
                    for ($i=0;$i<count($text);$i++) {
                        $fin = $fin . "<input type='$type' name='$name' id='$i' value='$i'><label for='$i'>$text[$i]</label><br>";
                    }
                    break;
                case inputType::option:
                    $fin = $fin . "<select name='$name'>";
                    for ($i=0;$i<count($text);$i++) {
                        $fin = $fin . "<option value='$i'>$text[$i]</option>";
                    }
                    $fin = $fin . "</select>";
                    break;
                default:
                    $type = "ERROR 02";
            }
            return "$fin<br>";
        }

        // * FUA *
            // simplify how to style html output universally for any submission format
        function readSingleInput($headerHTML, $inputHTML, $submitHTML, $nameArray) {
            $issue = False;
            $fin = array();
            // var_dump($nameArray);
            foreach($nameArray as $index => $name) {
                if (!isset($_GET[$index]) || empty($_GET[$index])) {
                        $issue = True;
                        break;
                } else {
                        $fin[$index] = $_GET[$index];
                }
            }
            if ($issue) {
                echo "
                    <center>
                        $headerHTML
                    </center>
                    <form method='GET'>
                        $inputHTML
                        $submitHTML
                    </form>
                    <br>
                    <center>
                        <i>Please fill up all fields before submission!</i>
                    </center>
                ";
            } else {
                var_dump($fin);
            }
        }

        function readMultipleInput($headerHTML, $inputHTML, $submitHTML, $name="filler") {
            if (!isset($_GET[$name])) {
                echo "
                    <center>
                        $headerHTML
                    </center>
                    <form method='GET'>
                        $inputHTML
                        $submitHTML
                    </form>
                    <center>
                        <i>Please fill up all fields before submission!</i>
                    </center>
                ";
            } else {
                var_dump($_GET[$name]);
            }
        }

        // ---------- CODE RUNNING ----------

        // ---------- PRESETS ----------

        $sampleArray = ["nice", "watermelon", "thank you sir", "okay bet", "the wall", "and", "the meme machine"];
        $headerHTML = createHeader();

        // $textInputHTML = "";
        // foreach ($sampleArray as $index => $item) {
        //     $textInputHTML = $textInputHTML . createSingleInput(inputType::text, $item, $index);
        // }

        // $passwordInputHTML = "";
        // foreach ($sampleArray as $index => $item) {
        //     $passwordInputHTML = $passwordInputHTML . createSingleInput(inputType::password, $item, $index);
        // }

        // $numberInputHTML = "";
        // foreach ($sampleArray as $index => $item) {
        //     $numberInputHTML = $numberInputHTML . createSingleInput(inputType::number, $item, $index);
        // }

        // $emailInputHTML = "";
        // foreach ($sampleArray as $index => $item) {
        //     $emailInputHTML = $emailInputHTML . createSingleInput(inputType::email, $item, $index);
        // }

        // $fileInputHTML = "";
        // foreach ($sampleArray as $index => $item) {
        //     $fileInputHTML = $fileInputHTML . createSingleInput(inputType::file, $item, $index);
        // }

        // $dateInputHTML = "";
        // foreach ($sampleArray as $index => $item) {
        //     $dateInputHTML = $dateInputHTML . createSingleInput(inputType::date, $item, $index);
        // }

        // $rangeInputHTML = "";
        // foreach ($sampleArray as $index => $item) {
        //     $rangeInputHTML = $rangeInputHTML . createSingleInput(inputType::range, $item, $index);
        // }

        // $timeInputHTML = "";
        // foreach ($sampleArray as $index => $item) {
        //     $timeInputHTML = $timeInputHTML . createSingleInput(inputType::time, $item, $index);
        // }

        // $colorInputHTML = "";
        // foreach ($sampleArray as $index => $item) {
        //     $colorInputHTML = $colorInputHTML . createSingleInput(inputType::color, $item, $index);q
        // }

        $radioInputHTML = createMultipleInput(inputType::radio, $sampleArray);
        // $optionInputHTML = createMultipleInput(inputType::option, $sampleArray);

        $submitInput = createSubmitInput();

        // ---------- RUNTIME CODE -----------

        // readSingleInput($headerHTML, $textInputHTML, $submitInput, $sampleArray);
        readMultipleInput($headerHTML, $radioInputHTML, $submitInput);

    ?>
</body>
</html>