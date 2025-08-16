import { createSignal } from "solid-js";
import { useNavigate } from "@solidjs/router";

function ArtistSearch() {
  const [artistName, setArtistName] = createSignal("");
  const navigate = useNavigate();

  const handleSubmit = (e) => {
    e.preventDefault();
    navigate(`/events/${artistName()}`);
  };

  return (
    <div>
      <h2>Search for an Artist</h2>
      <form onSubmit={handleSubmit}>
        <input
          type="text"
          value={artistName()}
          onInput={(e) => setArtistName(e.target.value)}
          placeholder="Enter artist name"
        />
        <button type="submit">Search</button>
      </form>
    </div>
  );
}

export default ArtistSearch;