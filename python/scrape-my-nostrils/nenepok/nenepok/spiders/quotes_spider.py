import scrapy

class QuotesSpider(scrapy.Spider): # class definition

    name = "quotes"

    start_urls = [
        'http://quotes.toscrape.com/page/1/',
    ]

    def parse(self, response):

        for quote in response.css('div.quote'): # div is the overall PARENT CONTAINER html element, quote is its class

            yield { # yield to return what looks like a JSON object
                'text': quote.css('span.text::text').get(), # span is the PARENT html element, text is the class, text is the meta element
                'author': quote.css('small.author::text').get(), # small is the PARENT html element, author is the class, text is the meta element
                'tags': quote.css('div.tags a.tag::text').getall(), # div is the PARENT html element, tags is the class, anchor is the CHILD html element, tag is its class, text is the meta element of the child class
            }

        next_page = response.css('li.next a::attr(href)').get() # list is the PARENT html element, next is its class, anchor is the CHILD html element where we want to extract its href value

        if next_page is not None: # break condition for recursive function call
            yield response.follow(next_page, self.parse) # recursive function call until no more next page
