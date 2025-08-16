<?php

class ConnectionManager(){

    public function getConnection(){
        $dsn = "mysql:host=localhost;dbname=ftl;port=3306";
        $pdo = new PDO($dsn, "root", "");
        return $pdo
    }

}

?>