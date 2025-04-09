// useState() is a built-in function that controls state
// useEffect() is another built-in function that automatically runs whenever the specified second arguments change
import {useEffect, useState} from "react" 
import "./styles.css" // import styles.css file from the same root folder
import {NewTodoForm} from "./NewTodoForm" // from the newtodoform

// you can only ever return ONE element in react
// fragment = jsx element with no tag inside of it that bypasses the restriction of returning multiple jsx elements from one component
// all component files must end with the .jsx file extension

// functions can be nested within components
export default function App() {

  // useState() is very versatile since it essentially is the equivalent of a PHP superglobal that maintains session state across react sessions
  // tuple destructuring that returns a variabel and a function
  // setX, setY, setZ are just user-defined setter functions that set a given value within react, and they will become incredibly important for use in React since every change requires a reassignment since initial state is immutable by default
  // && allows short circuitng in line 72

  // ----- 

  // these are called HOOKS!!!

  const [todods, setTodos] = useState(() => {
    const localValue = localStorage.getItem("ITEMS") // the localStorage API is a javascript specific concept that allows persisent storage of values through cookies as json objects in the browser
    if (localValue == null) {
      return [] // also returned to the todods variable
    } else {
      return JSON.parse(localValue) // returned to the todods variable
    }
  }) 

  useEffect(() => {
    localStorage.setItem("ITEMS"), JSON.stringify(todods) // this function is run, which effectively saves the todo array to local storage
  }, [todods]) // runs whenever the specified second arugment, the array of todods changes

  // ----- 

  function AddToDo(title){

    setTodos((currentTodos) => { // lowkey this is kinda functional init, assigning a function call to a variable like an ANONYMOUS function
      return [
        ...currentTodos, // destructuring and spreading out all existing todos in the array
        { // new todo element
          id: crypto.randomUUID(),
          title: title, // we are assiging it here already
          completed: false
        }
      ]
    })

  }

  function toggleToDo(id, completed){ // functional paradigms are adhered to because by default state is immutable in react
    setTodos(currentToDos => {
      return currentToDos.map(todo => {
        if (todo.id === id){
          return ({...todo, completed})
        }
        return todo
      })
    })
  }

  function deleteToDo(id) { // simulates deletion of an item since an item that has the same id will simply not be included
    setTodos(currentToDos => {
      return currentToDos.filter(todo => todo.id !== id)
    })
  }

  return (
    <>


      <NewTodoForm 
        onSubmit={AddToDo} 
      >
      </NewTodoForm> {/* cusotm react component defined by us can also have custom COMPONENTS added to them, here the custom component is a addToDo that receives the addtodo function*/}

      {/* list display */}
      <h1 className='header'>ToDoList</h1>
      <ul>
        {todods.length === 0 && "No Todos"}
        {todods.map( // each eachtodo iteration variable has the automatically assigned unique ID attribute that can be called
          eachToDo => {
            return (

            <li key={eachToDo.id}>
              <label>
                <input 
                type='checkbox' 
                checked={eachToDo.completed}
                onChange = { anotherGivenEvent => toggleToDo(eachToDo.id, anotherGivenEvent.target.check)} // calls another user-defined event when the checkbox is changed
                >
                </input>
                {eachToDo.title}
              </label>
              <button 
              className='btn btn-danger'
              onClick = {
                () => deleteToDo(eachToDo.id) // this is a function call
                // deleteToDo(todo.id) // this is an immediate function call and will pass the RETURN function value which is nothing
              }
              >Delete</button>
            </li>
            )
          }
        )}
      </ul>
    </>
  )

}