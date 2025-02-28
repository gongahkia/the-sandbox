import { component$ } from '@builder.io/qwik';
import { AgeEstimator } from '~/components/AgeEstimator';

export default component$(() => {
  return (
    <div class="container mx-auto">
      <AgeEstimator />
    </div>
  );
});