interface CarparkInfo {
  carpark_info: Array<{
    total_lots: string
    lot_type: string
    lots_available: string
  }>
  carpark_number: string
  update_datetime: string
}

interface ApiResponse {
  items: Array<{
    timestamp: string
    carpark_data: CarparkInfo[]
  }>
}

export async function getCarparkData() {
  const response = await fetch("https://api.data.gov.sg/v1/transport/carpark-availability")
  const data: ApiResponse = await response.json()

  return data.items[0].carpark_data.map((carpark) => ({
    carpark_number: carpark.carpark_number,
    total_lots: Number.parseInt(carpark.carpark_info[0].total_lots),
    lots_available: Number.parseInt(carpark.carpark_info[0].lots_available),
  }))
}