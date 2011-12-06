getIPCountry =
  # 
  #  getIPCountry(c("www.omegahat.org", "128.32.135.25", "www.google.com"))
  #  getIPCountry(c("www.omegahat.org", "www.google.com"))
  #  getIPCountry(c("169.237.46.32", "128.32.135.26"))
  #
function(ip, ..., curl = getCurlHandle(), stringsAsFactors = default.stringsAsFactors(),
          byHostName = grepl("[a-z]", ip))
{
  ifinfoQuery(ip, curl = curl, stringsAsFactors = stringsAsFactors, FALSE, byHostName = byHostName,
              c( "http://ipinfodb.com/ip_query2_country.php", "http://ipinfodb.com/ip_query_country.php"), ...)
}  

getIPLocation =
  # 
  #  getIPLocation(c("www.omegahat.org", "128.32.135.25", "www.google.com"))
  #  getIPLocation(c("www.omegahat.org", "www.google.com"))
  #  getIPLocation(c("169.237.46.32", "128.32.135.26"))
  #
function(ip, ..., curl = getCurlHandle(), stringsAsFactors = default.stringsAsFactors(),
           byHostName = grepl("[a-z]", ip))
{
  x = ifinfoQuery(ip, curl = curl, stringsAsFactors = stringsAsFactors, FALSE, byHostName = byHostName,
                    urls = c( "http://ipinfodb.com/ip_query2.php", "http://ipinfodb.com/ip_query.php"), ...)

  x[c("Latitude", "Longitude")] = lapply(x[c("Latitude", "Longitude")], function(x) as.numeric(as.character(x)))
  x
}


ifinfoQuery =
  #
  # This is the common worker function.
  # It splits the ips into host names and dotted-quad addresses and works on these as two separate groups.
  # It breaks the ips into groups of 25 or fewer and makes the requests for each block in separate requests
  # and merges the results back.
  
function(ip, curl = getCurlHandle(), stringsAsFactors = default.stringsAsFactors(),
          multi.part = FALSE, byHostName = grepl("[a-z]", ip), urls, ...)  
  
{
 
         # figure out which ip addresses are given as addresses and which are hostnames.
         # If they are not homogeneous, we have to use different query URLs to get the results
         # for the different groups.
      if(length(byHostName) > 1 && length(table(byHostName)) > 1) {

         h = ifinfoQuery(ip[byHostName], ..., curl = curl, stringsAsFactors = FALSE, multi.part = TRUE, byHostName = TRUE, urls = urls)
         i = getIPCountry(ip[!byHostName], ..., curl = curl, stringsAsFactors = FALSE, multi.part = TRUE, byHostName = FALSE, urls = urls)
         ans = rbind(h, i)
         return(if(!stringsAsFactors)
                   as.data.frame(lapply(ans, as.character), stringsAsFactors = FALSE)
                else
                  ans)

       }
          

  
   multi = length(ip) > 1

     # limit of 25 per call so group the ip values into groups of 25 or less.
   if(multi && length(ip) > 25) {
      vals = lapply(makeGroups(ip), ifinfoQuery, curl = curl, stringsAsFactors = FALSE, multi.part = TRUE, urls = urls, byHostName = byHostName)
      ans = as.data.frame(do.call(rbind, vals),
                           row.names = 1:length(ip),
                           stringsAsFactors = stringsAsFactors)
      return(ans)
   }
   
   u = if(multi || byHostName) 
          urls[1]
        else
          urls[2]

   
   txt = getForm(u, ip = paste(ip, collapse = ","), timezone = "false", curl = curl, ...)

   doc = xmlParse(txt, asText = TRUE)
   r = xmlRoot(doc)   
   if(multi) {
     ans = xmlApply(r, function(x) xmlSApply(x, xmlValue))
     m =  do.call(rbind, ans)
     if(multi.part)
        m
     else
        data.frame(m, stringsAsFactors = stringsAsFactors, row.names = 1:length(ip))
   } else {
     as.data.frame(xmlApply(r, xmlValue))
   }
 }

makeGroups =
  #
  # Uses to split collection of ips into groups of 25.
  #
function(ip)
{
      n = length(ip)
      num = n%/%25
      ans = split(ip, gl(num + 1, 25)[1:n])
      ans[sapply(ans, length) > 0]
}





