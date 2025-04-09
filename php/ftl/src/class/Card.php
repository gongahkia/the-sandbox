<?php

class Card {

    // FUA
        // continue adding required code and functionality here
        // anki calculation based on https://gist.github.com/fasiha/31ce46c36371ff57fdbc1254af424174

    private $name;
    private $description;
    private $daysToNextReview;
    private $DateNextReview;
    private $ratedDifficulty; // 1 => fail, 2=> pass but hard, 3 => pass and ok, 4 => pass and easy 
    private $memFactor; // 0 < memFactor < 1300, lower memFactor worse, higher memFactor better

    public function __construct($name, $description, $dateNextReview, $daysToNextReview, $ratedDifficulty, $memFactor=0){

        // reassignable by program
        $this->name = $name;
        $this->description = $description;
        $this->dateNextReview = $dateNextReview;
        $this->daysToNextReview = $daysToNextReview;
        $this->ratedDifficulty = $ratedDifficulty;
        $this->memFactor = $memFactor;

    }
    
    // ----- getter setter methods -----

    public function getName(){
        return $this->name;
    }

    public function getDescription(){
        return $this->description;
    }

    public function getDaysToNextReview(){
        return $this->daysToNextReview;
    }

    public function getDateNextReview(){
        return $this->dateNextReview;
    }

    public function getRatedDifficulty(){
        return $this->ratedDifficulty;
    }

    public function getMemFactor(){
        return $this->memFactor;
    }

    public function setName($name){
        $this->name = $name;
    }

    public function setDescription($description){
        $this->description = $description;
    }

    public function setDaysToNextReview($daysToNextReview){
        $this->daysToNextReview= $daysToNextReview;
    }

    public function setDateNextReview($dateNextReview){
        $this->dateNextReview = $dateNextReview;
    }

    public function setRatedDifficulty($ratedDifficulty){
        $this->ratedDifficulty = $ratedDifficulty;
    }

    public function setMemFactor($memFactor){
        $this->memFactor = $memFactor;
    }

    // ----- actual methods -----

    public function updateDaysToNextReview(){

        // check existing memFactor
        if ($this->getMemFactor() <= 0){
            $this->setMemFactor(0);
        } elseif (!$this->getMemFactor() - 300 <= 0){
            $this->setMemFactor($this->getMemFactor() - 300);
        } else {}

        // update memFactor based on ratedDifficulty
        $newMemFactor = match ($this->getRatedDifficulty()){
                    1 => 100,
                    2 => 225,
                    3 => 475,
                    4 => 650,
                } + $this->getMemFactor();

        // update memFactor and daysToNextReview
        $this->setMemFactor($newMemFactor);
        $this->setDaysToNextReview($this->getMemFactor() % 150);

    }

    public function updateDateNextReview(){

        $currDate = date("Y.m.d");
        $inter = (intval(explode(".", $currDate)[0]) * 365) + (intval(explode(".", $currDate)[1]) * 31) + (intval(explode(".", $currDate)[2])) + $this->getDaysToNextReview();
        $numYear = intdiv($inter, 365);
        $numMonth = intdiv(($inter % 365), 31);
        $numDays = $inter % 365) % 31;
        $finDate = "$numYear.$numMonth,$numDays"; // FUA rework this when I understand what format sql dates are to be stored in if needed
        $this->setDateNextReview($finDate);

    }


    public function editCardName(){
        // FUA add implementation
    }

    public function editCardDescription(){
        // FUA add implementation
    }

}

?>