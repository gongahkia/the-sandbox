// ~ shit monkeytype that's just one quote ---

// ----

// basically i want this to be like monkeytpe
// and when you finish typing the quote a picture of that scene from the anime will show huat ah

// -----

import { useEffect, useState } from 'react'
import './main.css'

export default function App() {

  // ~ other shit

  const [completed, setCompleted] = useState(false)
  const givenQuote = "Are you the strongest because you're Satoru Gojo, or are you Satoru Gojo because you're the strongest?"

  // ~ hooks

  const [untyped, setUntyped] = useState(() => {
    const localUntyped = localStorage.getItem("UNTYPED")
    if (localUntyped === null){
      return givenQuote
    } else {
      return JSON.parse(localUntyped)
    }
  })

  const [typed, setTyped] = useState(() => {
    const localTyped = localStorage.getItem("TYPED")
    if (localTyped === null){ // first initialisation so nothing found
      return ""
    } else { // something found
      return JSON.parse(localTyped)
    }
  })

  useEffect(() =>  {
      localStorage.setItem("TYPED", JSON.stringify(typed))
      localStorage.setItem("UNTYPED", JSON.stringify(untyped))
    }, 
    [typed, untyped]
  )

  // ~ actual code

  function generateUntyped(value) {
    setUntyped(givenQuote.substring(value.length));
  }


  function checkProgress(event) {
      const { value } = event.target;
      setTyped(value);
      generateUntyped(value);
      if (value === givenQuote) {
        setCompleted(true);
      } else {
        setCompleted(false);
      }
    }

    function resetPage() {
      setTyped("");
      setUntyped(givenQuote);
      setCompleted(false);
      localStorage.removeItem("TYPED");
      localStorage.removeItem("UNTYPED");
    }

  return (

    <>
    
    {/* header */}

    <h1 className='header'>a simple typeracer game</h1>

    <hr></hr>


    {/* show dynamic quote completion */}

    <div className='flexbox'>
      <span className='progressTyped'>
        {typed}
      </span>
      <span className='progressUntyped'>
        {untyped}
      </span>
    </div>

    <hr></hr>

    {/* user input */}

    <div className='flexbox'>

      {!completed && (
        <textarea 
          className='userInput' 
          onInput={checkProgress}
          value={typed}
        />
      )}

      {
        completed && (
          <img
            src='https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQHOa3ICPGUWbzdws_YMvOgHDzoG4ZSMagVOw&s'
          />

      )}

    </div>

    <div className='flexbox'>

      {
        completed && (

            <button onClick={resetPage}>try again!</button>

        )

      }

    </div>


    </>

  )

}