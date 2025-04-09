'use client';

import { useUser } from '@clerk/nextjs';

export default function UserComponent() {
  const { user } = useUser();
  return (
    <div>
      {user ? `Welcome, ${user.firstName}!` : 'Please sign in'}
    </div>
  );
}