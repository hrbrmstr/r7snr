
`r7snr` : Tools to work with Rapid7 scans.io Sonar Data

The following functions are implemented:

-   `snr_parse_response`: Parse Sonar HTTP study encoded `data` response.

### Installation

``` r
devtools::install_github("hrbrmstr/r7snr")
```

### Usage

This R package will let you work directly with the gzip'd JSON from Rapid7 scans.io Sonar HTTP studies.

Be warned that these are HUGE files and it's very likely this won't fit into memory on any system.

``` r
library(r7snr)

# current verison
packageVersion("r7snr")
```

    ## [1] '0.1.0'

``` r
library(r7snr)
library(jsonlite)
library(purrr)
library(dplyr)
library(purrr)
```

For this example, we'll grab the first 10 records from the 2016-07-05 HTTP study. It may report a message of "gzcat: error writing to output: Broken pipe" that we can ignore

``` r
system("curl --silent 'https://scans.io/data/rapid7/sonar.http/20160705-http.gz' | gzcat | head -100",
       intern=TRUE) %>%
  map_df(fromJSON) -> http_scan_records
```

We can take a look a the scan records to find that we have the:

-   virtual host name (this will have real virtual host names in forthcoming studies
-   the host (both of these are reported by zmap/zgrab)
-   data, which is a base 64 encoded version of the HTTP response (up to 32K)
-   port, which is the port the grabber connected to
-   ip, the IP address the grabber connected to

``` r
glimpse(http_scan_records)
```

    ## Observations: 100
    ## Variables: 5
    ## $ vhost (chr) "104.28.133.5", "192.190.87.148", "104.19.80.127", "23.10.145.164", "209.53.67.17", "23.15.234.254", ...
    ## $ host  (chr) "104.28.133.5", "192.190.87.148", "104.19.80.127", "23.10.145.164", "209.53.67.17", "23.15.234.254", ...
    ## $ data  (chr) "SFRUUC8xLjEgNDAzIEZvcmJpZGRlbg0KRGF0ZTogVGh1LCAwNyBKdWwgMjAxNiAwMDo0NToxNCBHTVQNCkNvbnRlbnQtVHlwZTog...
    ## $ port  (chr) "80", "80", "80", "80", "80", "80", "80", "80", "80", "80", "80", "80", "80", "80", "80", "80", "80",...
    ## $ ip    (chr) "104.28.133.5", "192.190.87.148", "104.19.80.127", "23.10.145.164", "209.53.67.17", "23.15.234.254", ...

We can examine one of them:

``` r
str(snr_parse_response(http_scan_records$data[10]))
```

    ## List of 1
    ##  $ :List of 4
    ##   ..$ status : int 200
    ##   ..$ version: chr "HTTP/1.1"
    ##   ..$ headers:List of 8
    ##   .. ..$ date          : chr "Thu, 07 Jul 2016 00:45:12 GMT"
    ##   .. ..$ server        : chr "Apache/2.2.10 (Unix) mod_ssl/2.2.10 OpenSSL/0.9.8i DAV/2 mod_auth_passthrough/2.1 mod_bwlimited/1.4"
    ##   .. ..$ last-modified : chr "Thu, 05 Dec 2013 22:54:10 GMT"
    ##   .. ..$ etag          : chr "\"6b502be-6f-4ecd1685ba880\""
    ##   .. ..$ accept-ranges : chr "bytes"
    ##   .. ..$ content-length: chr "111"
    ##   .. ..$ connection    : chr "close"
    ##   .. ..$ content-type  : chr "text/html"
    ##   .. ..- attr(*, "class")= chr [1:2] "insensitive" "list"
    ##   ..$ body   : chr "<html><head><META HTTP-EQUIV=\"refresh\" CONTENT=\"0;URL=/cgi-sys/defaultwebpage.cgi\"></head><body></body></html>\n"

Or, we can turn them all into a data frame:

``` r
map_df(1:nrow(http_scan_records), function(i) {
  x <- http_scan_records[i,]
  resp <- snr_parse_response(x$data)[[1]]
  data_frame(
    vhost=x$vhost, 
    host=x$host,
    port=x$port,
    ip=x$ip,
    status=resp$status,
    version=resp$version,
    body=resp$body,
    headers=list(resp$headers, stringsAsFactors=FALSE)
  )
}) -> parsed_records
```

Now we have a fairly usable data frame.

``` r
glimpse(parsed_records)
```

    ## Observations: 200
    ## Variables: 8
    ## $ vhost   (chr) "104.28.133.5", "104.28.133.5", "192.190.87.148", "192.190.87.148", "104.19.80.127", "104.19.80.127...
    ## $ host    (chr) "104.28.133.5", "104.28.133.5", "192.190.87.148", "192.190.87.148", "104.19.80.127", "104.19.80.127...
    ## $ port    (chr) "80", "80", "80", "80", "80", "80", "80", "80", "80", "80", "80", "80", "80", "80", "80", "80", "80...
    ## $ ip      (chr) "104.28.133.5", "104.28.133.5", "192.190.87.148", "192.190.87.148", "104.19.80.127", "104.19.80.127...
    ## $ status  (int) 403, 403, 200, 200, 403, 403, 400, 400, 302, 302, 400, 400, 200, 200, 302, 302, 400, 400, 200, 200,...
    ## $ version (chr) "HTTP/1.1", "HTTP/1.1", "HTTP/1.1", "HTTP/1.1", "HTTP/1.1", "HTTP/1.1", "HTTP/1.0", "HTTP/1.0", "HT...
    ## $ body    (chr) NA, NA, "<html><head><META HTTP-EQUIV=\"refresh\" CONTENT=\"0;URL=/cgi-sys/defaultwebpage.cgi\"></h...
    ## $ headers (list) Thu, 07 Jul 2016 00:45:14 GMT, text/html; charset=UTF-8, chunked, close, __cfduid=d7329deb551cf426...

We can now see the server types for what we've read in. Since that is not a required header, it may not be there so we have to handle `NULL` values.

``` r
map(parsed_records$headers, "server") %>% 
  map_chr(function(x) ifelse(length(x)>0, x, NA)) %>% 
  table(exclude=FALSE) %>% 
  as.data.frame(stringsAsFactors=FALSE) %>% 
  setNames(c("type", "count")) %>% 
  arrange(desc(count))
```

    ##                                                                                                   type count
    ## 1                                                                                                 <NA>   112
    ## 2                                                                                          AkamaiGHost    30
    ## 3                                                                                               Apache    14
    ## 4                                                                                                          6
    ## 5                                                                                         GoAhead-Webs     4
    ## 6                                                                                    Microsoft-IIS/6.0     3
    ## 7                                                                                                nginx     3
    ## 8                                                                                     cloudflare-nginx     2
    ## 9                                                                                    Microsoft-IIS/7.5     2
    ## 10                                                                                             alphapd     1
    ## 11                                                                                   Apache-Coyote/1.1     1
    ## 12                                                                                Apache/2.0.64 (Unix)     1
    ## 13 Apache/2.2.10 (Unix) mod_ssl/2.2.10 OpenSSL/0.9.8i DAV/2 mod_auth_passthrough/2.1 mod_bwlimited/1.4     1
    ## 14                                   Apache/2.2.11 (Win32) mod_jk/1.2.27 mod_ssl/2.2.11 OpenSSL/1.0.2c     1
    ## 15                                                                              Apache/2.2.15 (CentOS)     1
    ## 16                                                                              Apache/2.2.22 (Debian)     1
    ## 17                                                                              Apache/2.2.22 (Ubuntu)     1
    ## 18                            Apache/2.2.31 (FreeBSD) mod_ssl/2.2.31 OpenSSL/0.9.8zd-freebsd PHP/4.4.5     1
    ## 19           Apache/2.2.31 (Unix) mod_ssl/2.2.31 OpenSSL/1.0.1e-fips mod_bwlimited/1.4 mod_fcgid/2.3.9     1
    ## 20                                                Apache/2.4.6 (CentOS) OpenSSL/1.0.1e-fips PHP/5.4.16     1
    ## 21                                                                    Apache/2.4.6 (CentOS) PHP/5.4.16     1
    ## 22                                                                               Apache/2.4.7 (Ubuntu)     1
    ## 23                                                                                               BigIP     1
    ## 24                                                                                          CloudFront     1
    ## 25                                                                                     http server 1.0     1
    ## 26                                                                                   Microsoft-IIS/5.0     1
    ## 27                                                                                   Microsoft-IIS/7.0     1
    ## 28                                                                                   Microsoft-IIS/8.5     1
    ## 29                                                                      mini_httpd/1.19/bhoc 23sep2004     1
    ## 30                                                                                        nginx/1.10.0     1
    ## 31                                                                                        nginx/1.10.1     1
    ## 32                                                                                nginx/1.4.6 (Ubuntu)     1
    ## 33                                                                                       Tengine/2.0.0     1

If you're going to use this to process the Sonar HTTP studies, it's suggested you will want to use the [`jqr`](https://cran.rstudio.com/web/packages/jqr/) package to filter the downloaded gzip'd JSON file (which does mean learning the `jq` [filter syntax](https://stedolan.github.io/jq/manual/v1.4/)).

An alternative to using this R package is to follow the example on the [Sonar Wiki](https://github.com/rapid7/sonar/wiki/HTTP) and generate a HUGE JSON file from the results (NOTE: this will be over 1TB when some of our newer scans start to be posted). Then, use either `jqr` or `jq` on the command-line to extract fields you want/need and *then* process the resultant JSON in R.
