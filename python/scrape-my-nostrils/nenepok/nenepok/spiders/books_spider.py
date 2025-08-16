import scrapy

class BooksSpider(scrapy.Spider):
    name = "books"
    start_urls = [
        "http://books.toscrape.com",
    ]
    def parse(self, response):
        for shit in response.css('ol.row'): 
            yield {
                'book_name': shit.css('article.product_pod div.image_container a::attr(href)').get(),
                'number_of_stars': shit.css('article.product_pod p.star-rating::attr(class)').get().split()[-1], 
                'price': shit.css('article.product_pod div.product_price p.price_color::text').get(),
                'availability': shit.css('div.product_price p.instock.availability::text').getall()[-1].strip(),
            }
        next_page = response.css('li.next a::attr(href)').get() 
        if next_page is not None: 
            yield response.follow(next_page, self.parse) 