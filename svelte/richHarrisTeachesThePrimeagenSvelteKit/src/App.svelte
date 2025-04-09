<!-- note there can only ever be one instance of a script tag in a given svelte file -->
<script>
	import Counter from "./lib/Counter.svelte";
	import Worder from "./lib/Worder.svelte";
	import Coder from "./lib/Coder.svelte";
	let balls = "high";
	let balls2 = 1 + 1000000 * 20;
	let balls3 = "shiok bodo";
	let balls4 = 4;
</script>

<h1>watermelon sugar {balls}: it is time to learn SVELTE baby</h1>
<img src="https://media.tenor.com/9KtbGeFNkREAAAAM/dance-happy.gif" alt="happy happy happy">
<h2>we live in a society in the year {balls2}</h2>
<p>actually all that is rendering here is just {balls3.toUpperCase()} jsx so react knowledge is transferrable!</p>
<hr>
<div style='color:green;background-color:white'> <!-- styles in svelte are scoped locally to each individual svelte file -->
	<p>but also since its all jsx note that simple function evaluation is possible but more complex returns are not, we'll begin to see the limitations of this in a bit i think</p>
</div>
<hr>
<div style="font-family: 'Franklin Gothic Medium', 'Arial Narrow', Arial, sans-serif; color:coral">
	Below are {balls4} user-defined components within the 
	<pre>./src/lib</pre> 
	folder
</div>
<hr>
1. this component is called <b>Counter</b>
<br>
<Counter></Counter> <!-- defined and imported elsewhere from the lib directory -->

<hr>
2. this component is called <b>Worder</b>
<br>
<Worder></Worder>
<hr>
3. this component is called <b>Coder</b>
<br>
<Coder></Coder>
<hr>
<h2>Basically Svelte is the best and everyone should try it asap!</h2>
