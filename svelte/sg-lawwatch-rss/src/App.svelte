<script>
  import { onMount } from 'svelte';
  import { XMLParser } from 'fast-xml-parser';

  let items = [];
  let isLoading = true;
  let error = null;

  // Using CORS proxy
  const RSS_FEED_URL = 'https://api.allorigins.win/raw?url=https://www.singaporelawwatch.sg/Portals/0/RSS/SuperFeed.xml';
  
  const parser = new XMLParser({
    ignoreAttributes: false,
    attributeNamePrefix: '',
    parseAttributeValue: true,
    trimValues: true
  });

  onMount(async () => {
    try {
      const response = await fetch(RSS_FEED_URL);
      if (!response.ok) throw new Error(`HTTP error! status: ${response.status}`);
      
      const xmlText = await response.text();
      const parsedData = parser.parse(xmlText);
      items = parsedData?.rss?.channel?.item || [];
    } catch (err) {
      error = `Failed to load feed: ${err.message}`;
      console.error('Fetch error:', err);
    } finally {
      isLoading = false;
    }
  });
</script>

<style>
  .container {
    max-width: 1200px;
    margin: 0 auto;
    padding: 2rem 1rem;
    font-family: 'Segoe UI', system-ui, sans-serif;
  }

  .header {
    border-bottom: 3px solid #0055aa;
    margin-bottom: 2rem;
    padding-bottom: 1rem;
  }

  .card {
    background: white;
    border-radius: 8px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.1);
    margin-bottom: 1.5rem;
    padding: 1.5rem;
    transition: transform 0.2s ease;
  }

  .card:hover {
    transform: translateY(-2px);
    box-shadow: 0 4px 12px rgba(0,0,0,0.15);
  }

  .title {
    color: #0055aa;
    font-size: 1.25rem;
    margin: 0 0 0.5rem 0;
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }

  .title:before {
    content: '⚖️';
    font-size: 1.2em;
  }

  .meta {
    color: #666;
    font-size: 0.9rem;
    margin-bottom: 1rem;
    display: flex;
    gap: 1rem;
  }

  /* Updated CSS for dynamic content */
  .content-wrapper :global(p) {
    color: #444;
    line-height: 1.6;
    font-size: 0.95rem;
    margin: 0.5rem 0;
  }

  .content-wrapper :global(a) {
    color: #0055aa;
    text-decoration: none;
    border-bottom: 1px solid transparent;
    transition: border-color 0.2s;
  }

  .content-wrapper :global(a:hover) {
    border-bottom-color: currentColor;
  }

  .loading {
    display: flex;
    flex-direction: column;
    gap: 1rem;
  }

  .skeleton {
    background: #f0f0f0;
    border-radius: 4px;
    height: 1.5rem;
    animation: pulse 1.5s infinite;
  }

  @keyframes pulse {
    0%, 100% { opacity: 1 }
    50% { opacity: 0.5 }
  }
</style>

<div class="container">
  <div class="header">
    <h1>Singapore Law Updates</h1>
    <p>Latest legal developments from Singapore Law Watch</p>
  </div>

  {#if isLoading}
    <div class="loading">
      {#each { length: 5 } as _}
        <div class="card">
          <div class="skeleton" style="width: 70%"></div>
          <div class="skeleton" style="width: 50%"></div>
          <div class="skeleton" style="width: 90%"></div>
        </div>
      {/each}
    </div>
  {:else if error}
    <div class="error">{error}</div>
  {:else}
    {#each items as item}
      <article class="card">
        <h2 class="title">
          <a href={item.link} target="_blank" rel="noopener">{item.title}</a>
        </h2>
        
        <div class="meta">
          {#if item['dc:creator']}
            <span>By {item['dc:creator']}</span>
          {/if}
          <span>{new Date(item.pubDate).toLocaleDateString('en-SG', {
            year: 'numeric',
            month: 'long',
            day: 'numeric'
          })}</span>
        </div>

        <div class="content-wrapper">
          {@html item.description}
        </div>
      </article>
    {/each}
  {/if}
</div>