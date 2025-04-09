import {useState} from "react"

export function NewTodoForm(props) {

  const [newItem, setNewItem] = useState("default sexy beast") 

  props.onSubmit

  function handleSubmit(aGivenEvent){ // this is just a normal js function
    aGivenEvent.preventDefault()

    if (newItem === ""){
      return
    } else {
      props.onSubmit(newItem)
    }

    setNewItem("") // clears out the existing text bar

  }

  return (

      <form onSubmit={handleSubmit} className="new-item-form">
        <div className="form-row">
          <label htmlFor='item'>New Item</label>
          <input 
          value= {newItem} 
          onChange={eventObject => setNewItem(eventObject.target.value)}  // basically convulated af but this line affects the line above
          type='text' 
          id='item'
          ></input>
        </div>
        <button className='btn'>Add</button>
      </form>
  )

}