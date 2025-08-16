<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Rmb</title>
</head>
<body>

<?php
// FUTURE IDEAS
    // display specific filtered items as their own list
    // allow searching of items via task descriptions by keywords
    // allow sorting of items by tags, be clear about intended logic before moving forward with this
    // work out a way to save data to local storage if that's a feature I want to incorporate
    // figure out if i want to integrate a mysql database into this

// ---------- presets ----------

// session initialisation

session_start();

if (!isset($_SESSION["data"])){
    $_SESSION["data"] = array();
    $_SESSION["tags"] = array();
}

// function definition

function sortByUrgency($storedData){
    $low = [];
    $medium = [];
    $high = [];
    foreach($storedData as $task){
        $taskName = $task["taskName"];
        $taskUrgency = $task["taskUrgency"];
        $taskTags = $task["taskTags"];
        switch ($taskUrgency){ // no default case required since state modelling with invalid states being made unrepresentable handled by html radio buttons
            case "low":
                $low[] = $task;
            case "medium":
                $medium[] = $task;
            case "high":
                $high[] = $task;
        }
    }
    return [$low, $medium, $high];
}

function displayRemoveTaskForm($storedData){
    $rowSpan = count($storedData) + 2;
    echo "
    <table border='1'>
        <form action='index.php' method='POST'>
        <th colspan='5'>Delete task</th>
        <tr>
            <td rowspan='$rowSpan'><b>tasks</b></td>
        </tr>
        <tr>
            <td>Description</td>
            <td>Urgency</td>
            <td>Select</td>
        </tr>
        ";

    foreach($storedData as $task){
        $description = $task["taskName"];
        $urgency = $task["taskUrgency"];
        $tags = $task["taskTags"];
        echo "
        <tr>
            <td>$description</td>
            <td>$urgency</td>
            <td><input type='checkbox' name='deletedTask[]' value='$description'</td>
        </tr>
            ";
    }

    echo "
        <tr>
            <td colspan='5'>
                <input type='submit' value='delete task' name='deleteTask'></input>
            </td>
        </tr>
        </form>
        </table> 
    ";
}

// FUA
function displayRemoveTagsForm($tags){
    $rowSpan = count($tags)+1;
    echo "
    <table border='1'>
        <form action='index.php' method='POST'>
        <th colspan='2'>Delete tag</th>
        <tr>
            <td rowspan='$rowSpan'><b>tag names</b></td>
        </tr>
    ";

    foreach($tags as $tag){
        echo "
                <tr><td><label><input type='checkbox' name='deletedTag[]' value='$tag'></input>$tag<label></td></tr>
        ";
    }

    echo "
        <tr>
            <td colspan='2'><input type='submit' value='delete tag' name='deleteTag'></input></td>
        </tr>
        </form>
    </table></br>
    ";
}

function displayAddTagsForm($tags){
    echo "
    <table border='1'>
        <form action='index.php' method='POST'>
        <th colspan='2'>Add tag</th>
        <tr>
            <td><b>tag name</b></td>
            <td><input type='text' name='addedTag'></input></td>
        </tr>
        <tr>
            <td colspan='2'><input type='submit' name='addTag' value='add tag'></input></td>
        </tr>
        </form>
    </table><br>
    ";
}

function displayAddTaskForm($tags){
    $row = count($tags) + 1;
    echo "
    <table border='1'>
        <form action='index.php' method='POST'>
            <th colspan='2'>Add task</th>
            <tr>
                <td><b>task name</b></td>
                <td colspan='2'><input type='text' name='task'></input></td>
            </tr>
            <tr>
                <td><b>task urgency</b></td>
                <td colspan='2'>
                    <label><input type='radio' name='urgency' value='low' checked></input>low</label>
                    <label><input type='radio' name='urgency' value='medium'></input>medium</label>
                    <label><input type='radio' name='urgency' value='high'></input>high</label>
                </td>
            </tr>
            <tr>
                <td rowspan='$row'>
                    <b>task tags</b>
                </td>
            </tr>
            ";
    foreach($tags as $tag){
        echo "
            <tr><td><label><input type='checkbox' name='tags[]' value='$tag'></input>$tag</label></td></tr>
        ";
    }
    echo "
            <tr>
                <td colspan='2'><input type='submit' value='add task' name='addTask'></td>
            </tr>
        </form>
    </table><br>
    ";
}

function displayTasks($storedData){
    echo "
    <table border='1'>
    <tr>
        <th colspan='4'>Task</th>
    </tr>
    <tr>
        <th>number</th>
        <th>description</th>
        <th>urgency</th>
        <th>tags</th>
    </tr>
    ";
    foreach($storedData as $index => $taskDetail){
        $num = $index + 1;
        $taskName = trim($taskDetail["taskName"]);
        $taskUrgency= trim($taskDetail["taskUrgency"]);
        $taskTags= $taskDetail["taskTags"];
        $rowSpan = count($taskTags) + 1;

        if (count($taskTags) === 0){
            echo "
            <tr>
                <td rowspan='$rowSpan'>$num</td>
                <td rowspan='$rowSpan'>$taskName</td>
                <td rowspan='$rowSpan'>$taskUrgency</td>
                <td></td>
            </tr>
            ";
        } else {
            echo "
            <tr>
                <td rowspan='$rowSpan'>$num</td>
                <td rowspan='$rowSpan'>$taskName</td>
                <td rowspan='$rowSpan'>$taskUrgency</td>
            </tr>
            ";
            foreach($taskTags as $taskTag){
                echo "<tr><td>$taskTag</td></tr>";
            }
        }

    }
    echo "</table><br>";
}

function displayClearSessionDataForm(){
    echo "
    <table border='1'> 
        <form action='index.php' method='POST'>
        <th>Clear Session Data</th>
        <tr>
            <td><input type='submit' value='clear data' name='clearData'></input></td>
        </tr>
        </form>
    </table></br>
    ";
}

function defaultDisplay(){
    displayTasks($_SESSION["data"]);
    displayAddTaskForm($_SESSION["tags"]);
    displayAddTagsForm($_SESSION["tags"]);
    displayClearSessionDataForm();
    displayRemoveTagsForm($_SESSION["tags"]);
    displayRemoveTaskForm($_SESSION["data"]);
    var_dump(sortByUrgency($_SESSION["data"]));
}

// ---------- execution code ------------

// debugging warriors

// var_dump($_POST);
// var_dump($_SESSION); 
// var_dump($_SESSION["data"]);
// var_dump($_SESSION["tags"]);

if (isset($_POST["addTask"])){ // addTask button pressed

    if (empty($_POST["task"]) || !isset($_POST["urgency"]) || empty($_POST["urgency"])){ // empty task form submitted

        echo "<h1>Incomplete add task form</h1>";
        defaultDisplay();

    } else { // task form submitted with input

        $taskName = $_POST["task"];
        $taskUrgency = $_POST["urgency"];

        if (isset($_POST["tags"])){
            $taskTags = $_POST["tags"];
        } else {
            $taskTags = [];
        }

        $taskDetail = array(
            "taskName" => $taskName,
            "taskUrgency" => $taskUrgency,
            "taskTags" => $taskTags
        );

        echo "<h1>Task added</h1>";
        $_SESSION["data"][] = $taskDetail;
        defaultDisplay();

    }
} elseif (isset($_POST["addTag"])){ // addTag button pressed

    if (empty($_POST["addedTag"])){ // empty add tag form submitted

        echo "<h1>Incomplete add tag form</h1>";
        defaultDisplay();

    } else { // add tag form submitted with input

        echo "<h1>Tag added</h1>";
        $addedTag = $_POST["addedTag"];
        $_SESSION["tags"][] = $addedTag;
        defaultDisplay();

    }
} elseif (isset($_POST["clearData"])){ // clearData button pressed

    echo "<h1>Data cleared</h1>";
    $_SESSION["data"] = array();
    $_SESSION["tags"] = array();
    defaultDisplay();

}  elseif (isset($_POST["deleteTag"])){ // deleteTag button pressed
    if (empty($_POST["deletedTag"])){ // empty delete tag form submitted

        echo "<h1>Incomplete delete tag form</h1>";
        defaultDisplay();

    } else { // delete tag form submitted with input

        $tagsToDelete = $_POST["deletedTag"];
        $numTagsDeleted = count($tagsToDelete);

        if (count($tagsToDelete) === 1){
            echo "<h1>1 tag deleted</h1>";
        } else {
            echo "<h1>$numTagsDeleted tags deleted</h1>";
        }

        $shallowCopySessionTags = $_SESSION["tags"];
        $finSessionTags = array();
        $_SESSION["tags"] = array();
        foreach($shallowCopySessionTags as $tag){
            if (!in_array($tag, $tagsToDelete)){
                $finSessionTags[] = $tag;
            }
        }
        $_SESSION["tags"] = $finSessionTags;
        defaultDisplay();

    }
} elseif(isset($_POST["deleteTask"])) { // deleteTask button pressed
    if (empty($_POST["deletedTask"])){ // empty delete task form submitted

        echo "<h1>Incomplete delete task form</h1>";
        defaultDisplay();

    } else {

        echo "<h1>Tasks deleted</h1>";

        $tasksToDelete = $_POST["deletedTask"];
        $numTasksDeleted = count($tasksToDelete);

        if (count($tasksToDelete) === 1){
            echo "<h1>1 task deleted</h1>";
        } else {
            echo "<h1>$numTasksDeleted tasks deleted</h1>";
        }

        $shallowCopySessionTasks = $_SESSION["data"];
        $finSessionTasks = array();
        $_SESSION["data"] = array();

        foreach($shallowCopySessionTasks as $task){ // this essentially means that any task name with the same name will be deleted
            if (!in_array($task["taskName"],$tasksToDelete)){
                $finSessionTasks[] = $task;
            }
        }

        $_SESSION["data"] = $finSessionTasks;

        defaultDisplay();

    }
} else { // addTask, addTag, clearData, deleteTag and deleteTask button NOT pressed

    defaultDisplay();

}

?>
</body>
</html>
