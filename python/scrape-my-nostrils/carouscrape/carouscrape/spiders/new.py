import scrapy

class CarousellSpider(scrapy.Spider): 

    name = "carousell"

    start_urls = [
        "https://www.carousell.sg/search/bouldering/?addRecent=true&canChangeKeyword=true&includeSuggestions=true&searchId=Pnw8dg&t-search_query_source=direct_search"
    ]

    def parse(self, response):

# important divs
# 
# some divs to take note of
#
# D_mv => seller profile image
# D_jL D_jM D_jQ D_jS D_jW D_jZ D_ki => seller username
# D_jL D_jM D_jQ D_jS D_jW D_jY D_oQ D_kj => seller last seen status
#
# ----------
# 
# D_kl D_Ry => item image
# D_jL D_jM D_jQ D_jT D_jW D_jY D_jU D_ki => item name
# D_jL D_jM D_jQ D_jS D_jW D_jZ D_kh => item price
# D_jL D_jM D_jQ D_jS D_jW D_jY D_ki => item state (new, old, used)
# D_lq D_pf => like the item
# D_lq => more details
#
# ----------

        for quote in response.css('div.D_uR.D_ot'):

            yield {
                'text': quote.css('div[data-testid="listing-card-text-seller-name"]').get(),
            }

        next_page = response.css('button.D_kb.D_jT.D_kp.D_kk.D_jX.D_GN::attr(onclick)').get()

        if next_page is not None: # break condition for recursive function call
            yield response.follow(next_page, self.parse)
