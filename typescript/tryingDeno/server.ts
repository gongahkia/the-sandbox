import { serve } from "https://deno.land/std@0.150.0/http/server.ts";

const handler = (req: Request): Response => {
  return new Response("Hello World and Hello Deno!", {
    status: 200,
  });
};

console.log("Server running on http://localhost:8000");

await serve(handler);