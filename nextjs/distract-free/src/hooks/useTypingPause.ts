import { useEffect, useRef } from "react";

const useTypingPause = (
  onTyping: () => void,
  onPause: (duration: number) => void,
  onDistraction: () => void
) => {
  const lastTyped = useRef(Date.now());
  const pauseTimeout = useRef<NodeJS.Timeout | null>(null);

  useEffect(() => {
    const handleKeyDown = () => {
      const now = Date.now();
      if (pauseTimeout.current) clearTimeout(pauseTimeout.current);
      lastTyped.current = now;
      onTyping();
      pauseTimeout.current = setTimeout(() => {
        const pauseDuration = (Date.now() - lastTyped.current) / 1000;
        if (pauseDuration > 10) {
          onPause(pauseDuration);
          onDistraction();
        }
      }, 11000);
    };

    window.addEventListener("keydown", handleKeyDown);
    return () => window.removeEventListener("keydown", handleKeyDown);
  }, [onTyping, onPause, onDistraction]);
};

export default useTypingPause;