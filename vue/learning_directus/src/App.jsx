import { BrowserRouter, Routes, Route } from "react-router-dom";
import { useParams } from "react-router-dom";
import { useEffect, useState } from "react";
import directus from "./util/directus";
import { readItem } from "@directus/sdk";

function Page() {
  const { slug } = useParams();
  const [page, setPage] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState("");

  useEffect(() => {
    setLoading(true);
    async function fetchPage() {
      try {
        const data = await directus.request(readItem("pages", slug));
        setPage(data);
        setError("");
      } catch (err) {
        setError("Page not found");
      } finally {
        setLoading(false);
      }
    }
    fetchPage();
  }, [slug]);

  if (loading) return <p>Loading...</p>;
  if (error) return <h1 style={{ color: "red" }}>{error}</h1>;

  return (
    <div>
      <h1>{page.title}</h1>
      <div dangerouslySetInnerHTML={{ __html: page.content }} />
    </div>
  );
}

function App() {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/:slug" element={<Page />} />
      </Routes>
    </BrowserRouter>
  );
}

export default App;
