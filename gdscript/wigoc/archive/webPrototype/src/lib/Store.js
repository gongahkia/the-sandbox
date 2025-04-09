import { writable } from 'svelte/store';

/*
* ----- svlete/store -----
    * writable store: client-side local storage 
        * holds a value, allows READ, UPDATE and SUBSCRIBE to that value
        * used to 
            * managing state in Svelte apps
            * sharing state between components
    * functions 
        * writable(): CREATES a writable store with the intialised value
        * set(): SETS the writable store's value
        * update(): UPDATES the store's value
        * subscribe(): SUBSCRIBES to the store, reading the stored value and calling the specified callback function whenever the stored value changes
*/

// ----- VARIABLE INTIALISATION -----

// boardstate represented as an 3-dimensional array
const initialBoard = [
    ['c', 'n', 'b', 'k', 'k', 'b', 'n', 'c'],
    ['p', 'p', 'p', 'p', 'p', 'p', 'p', 'p'],
    ['', '', '', '', '', '', '', ''],
    ['', '', '', '', '', '', '', ''],
    ['', '', '', '', '', '', '', ''],
    ['', '', '', '', '', '', '', ''],
    ['P', 'P', 'P', 'P', 'P', 'P', 'P', 'P'],
    ['C', 'N', 'B', 'K', 'K', 'B', 'N', 'C']
];

// ----- MAIN EXECUTION CODE -----

export const boardState = writable(initialBoard); // write initial boardState to local storage
export const selectedPiece = writable(null); // writes initial selectedPiece to local storage with null value
export const whoseTurn = writable('white'); // writes initial active whoseTurn to local storage with value of "white"

// --- FUA add more code here for other values to be stored in the future --- 