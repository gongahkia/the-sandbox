FROM php:8.2-cli
RUN apt-get update && apt-get install -y \
    libzip-dev \
    && docker-php-ext-install zip
WORKDIR /app
COPY php-runner.sh .
CMD ["php", "php-runner.sh"]