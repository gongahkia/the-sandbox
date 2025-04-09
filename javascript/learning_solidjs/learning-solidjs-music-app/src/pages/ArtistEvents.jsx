import { createResource } from "solid-js";
import { useParams } from "@solidjs/router";
import axios from "axios";

const fetchArtistEvents = async (artistName) => {
  const response = await axios.get(`https://rest.bandsintown.com/artists/${artistName}/events?app_id=YOUR_APP_ID`);
  return response.data;
};

function ArtistEvents() {
  const params = useParams();
  const [events] = createResource(() => params.artistName, fetchArtistEvents);

  return (
    <div>
      <h2>Upcoming Events for {params.artistName}</h2>
      {events.loading && <p>Loading events...</p>}
      {events.error && <p>Error: {events.error.message}</p>}
      {events() && (
        <ul>
          {events().map((event) => (
            <li>
              {event.datetime} - {event.venue.name}, {event.venue.city}
            </li>
          ))}
        </ul>
      )}
    </div>
  );
}

export default ArtistEvents;
