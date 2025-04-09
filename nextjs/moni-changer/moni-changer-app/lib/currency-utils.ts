// Map of currency codes to flag emojis
const currencyFlags: Record<string, string> = {
  USD: "🇺🇸",
  EUR: "🇪🇺",
  GBP: "🇬🇧",
  JPY: "🇯🇵",
  AUD: "🇦🇺",
  CAD: "🇨🇦",
  CHF: "🇨🇭",
  CNY: "🇨🇳",
  HKD: "🇭🇰",
  NZD: "🇳🇿",
  SEK: "🇸🇪",
  KRW: "🇰🇷",
  SGD: "🇸🇬",
  NOK: "🇳🇴",
  MXN: "🇲🇽",
  INR: "🇮🇳",
  RUB: "🇷🇺",
  ZAR: "🇿🇦",
  TRY: "🇹🇷",
  BRL: "🇧🇷",
  TWD: "🇹🇼",
  DKK: "🇩🇰",
  PLN: "🇵🇱",
  THB: "🇹🇭",
  IDR: "🇮🇩",
  HUF: "🇭🇺",
  CZK: "🇨🇿",
  ILS: "🇮🇱",
  CLP: "🇨🇱",
  PHP: "🇵🇭",
  AED: "🇦🇪",
  COP: "🇨🇴",
  SAR: "🇸🇦",
  MYR: "🇲🇾",
  RON: "🇷🇴",
}

// Map of currency codes to full names
const currencyNames: Record<string, string> = {
  USD: "US Dollar",
  EUR: "Euro",
  GBP: "British Pound",
  JPY: "Japanese Yen",
  AUD: "Australian Dollar",
  CAD: "Canadian Dollar",
  CHF: "Swiss Franc",
  CNY: "Chinese Yuan",
  HKD: "Hong Kong Dollar",
  NZD: "New Zealand Dollar",
  SEK: "Swedish Krona",
  KRW: "South Korean Won",
  SGD: "Singapore Dollar",
  NOK: "Norwegian Krone",
  MXN: "Mexican Peso",
  INR: "Indian Rupee",
  RUB: "Russian Ruble",
  ZAR: "South African Rand",
  TRY: "Turkish Lira",
  BRL: "Brazilian Real",
  TWD: "Taiwan Dollar",
  DKK: "Danish Krone",
  PLN: "Polish Złoty",
  THB: "Thai Baht",
  IDR: "Indonesian Rupiah",
  HUF: "Hungarian Forint",
  CZK: "Czech Koruna",
  ILS: "Israeli Shekel",
  CLP: "Chilean Peso",
  PHP: "Philippine Peso",
  AED: "UAE Dirham",
  COP: "Colombian Peso",
  SAR: "Saudi Riyal",
  MYR: "Malaysian Ringgit",
  RON: "Romanian Leu",
}

export function getCurrencyFlag(currencyCode: string): string {
  return currencyFlags[currencyCode] || "🏳️"
}

export function getCurrencyName(currencyCode: string): string {
  return currencyNames[currencyCode] || "Unknown Currency"
}