# Godot  

* Everything in Godot is a Node  
* Nodes can contain other Nodes  
* Scenes can contain other Scenes  
* Node: multiple Nodes comprise a Scene  
* Scene: reusable building blocks comprised of Nodes, the equivalent of components in other web frontend frameworks like Svelte or React  
    * By default, Scene elements *(nodes, scenes)* are rendered sequentially top-down, meaning the first element at the top of the Scene is drawn first, last element at the bottom of the Scene is drawn last  
    * Draw order can be specified using the Z-index attribute *(under ordering)* of a given Scene element  
* `.tscn` are Scene files: save the state of each Godot Scene  
* `.gd` script are GDScript files: attached to Scenes to afford more complex logic   

## More on  

* [godot 4 platformer](https://youtu.be/LOhfqjmasi0?si=9IbwgyU7HwAXI_5F) by Brackeys  
* [godot 4 doom clone](https://youtu.be/jzbgH4AMtI8?si=6OKtVdeRwhBDsh9o) by Miziziziz  
* [godot 4 top down shooter](https://youtu.be/UYQfVx1EIW8?si=i0TpCmxqtvFsH6cd) by Miziziziz  
