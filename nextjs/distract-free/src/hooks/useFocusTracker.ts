import { useEffect } from "react";

const useFocusTracker = (onBlur: () => void) => {
  useEffect(() => {
    const handleBlur = () => onBlur();
    window.addEventListener("blur", handleBlur);
    return () => window.removeEventListener("blur", handleBlur);
  }, [onBlur]);
};

export default useFocusTracker;