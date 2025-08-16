import { Router, Route } from "@solidjs/router";
import Home from "./pages/Home";
import ArtistSearch from "./pages/ArtistSearch";
import ArtistEvents from "./pages/ArtistEvents";

function App() {
  return (
    <Router>
      <nav>
        <a href="/">Home</a>
        <a href="/search">Search Artists</a>
      </nav>
      <Route path="/" component={Home} />
      <Route path="/search" component={ArtistSearch} />
      <Route path="/events/:artistName" component={ArtistEvents} />
    </Router>
  );
}

export default App;