import { component$, useSignal, useResource$, Resource } from '@builder.io/qwik';

export const AgeEstimator = component$(() => {
  const name = useSignal<string>('');

  const ageResource = useResource$<{ name: string; age: number; count: number }>(
    async ({ track, cleanup }) => {
      track(() => name.value);
      const abortController = new AbortController();
      cleanup(() => abortController.abort('cleanup'));

      if (!name.value) return null;

      const res = await fetch(`https://api.agify.io?name=${encodeURIComponent(name.value)}`, {
        signal: abortController.signal,
      });
      return res.json();
    }
  );

  return (
    <div class="p-4">
      <h1 class="text-2xl font-bold mb-4">Age Estimator</h1>
      <input
        type="text"
        value={name.value}
        onInput$={(ev) => (name.value = (ev.target as HTMLInputElement).value)}
        placeholder="Enter a name"
        class="border p-2 rounded"
      />
      <Resource
        value={ageResource}
        onPending={() => <p>Estimating age...</p>}
        onRejected={(error) => <p>Error: {error.message}</p>}
        onResolved={(result) => (
          result && (
            <p class="mt-4">
              Estimated age for {result.name}: <strong>{result.age}</strong> years old
            </p>
          )
        )}
      />
    </div>
  );
});