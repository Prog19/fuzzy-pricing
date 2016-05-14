library(sets)
library(forecast)
# read dataset into a R data frame
ds = read.csv(file.path(getwd(), "fuzzy-pricing", "superstore_sales.tsv"), sep = '\t')
#names(ds)# dataset features
test.ds = ds[8299:8399,]
ds = ds[1:8299,]

#min max data normalisation
norm1 = function(attr)
{
  min.attr.val = min(attr)
  max.attr.val = max(attr)
  if(min.attr.val == max.attr.val)
   next
 (attr-min.attr.val)/(max.attr.val-min.attr.val)
}

norm2 = function(val,min,max)
{
 (val-min)/(max-min)
}

# extract values to construct fuzzy sets for input variables
# 1.DISCOUNT RATE
disc.col = ds[,which(names(ds) == "Discount")]
disc.col1 = norm1(disc.col)
discount.rate = quantile(disc.col1)
discount.rate
#   0%  25%  50%  75% 100% 
# 0.00 0.02 0.05 0.08 0.25 
# 0%  25%  50%  75% 100% 
# 0.00 0.08 0.20 0.32 1.00 
 
# 2. PRODUCT POPULARITY
prod.col = ds[,which(names(ds) == "Product.Name")]
prods = levels(prod.col)
prod.cnt = c()
for(i in 1:length(prods))
{
 #cat(prods[i], "\t", 
 #	length(which(prod.col == prods[i])), "\t", 
 #	which(prod.col == prods[i]), "\n")
 prod.cnt[i] = length(which(prod.col == prods[i])) 
}
min.prod.popu = min(prod.cnt)
max.prod.popu = max(prod.cnt)
prod.cnt1 = norm1(prod.cnt)
prod.popu = quantile(prod.cnt1)
#as.numeric(quantile(prod.cnt)) 
	# gets the values in a vector to substitute
prod.popu
# 0%  25%  50%  75% 100% 
#  1    4    6    9   24 
#   0%       25%       50%       75%      100% 
# 0.0000000 0.1666667 0.2500000 0.3750000 1.0000000 

get.prod.popu = function(prod)
{
 prod.cnt[which(prods == prod)[1]]
}
#get.prod.popu("Newell 323")


# 3. CATEGORY POPULARITY
category.col = ds[,which(names(ds) == "Product.Sub.Category")]
	# only 3 categories available, so sub categories are taken instead
categories = levels(category.col)
category.cnt = c()
for(i in 1:length(categories))
{
 category.cnt[i] = length(which(category.col == categories[i])) 
}
#categories = data.frame(categories,category.len)
#categories
min.category.popu = min(category.cnt)
max.category.popu = max(category.cnt)
category.cnt1 = norm1(category.cnt)
category.popu = quantile(category.cnt1)
category.popu
# 0%  25%  50%  75% 100% 
# 87  246  386  758 1225 
#        0%       25%       50%       75%      100% 
# 0.0000000 0.1398041 0.2626892 0.5939448 1.0000000 

get.prod.category.popu = function(prod)
{
 category = ds[which(prod.col == prod)[1],"Product.Sub.Category"]
 category.cnt[which(categories == category)[1]]
}
#get.prod.category.popu("Newell 323")


# 4. USER SALES
user.col = ds[,which(names(ds) == "Customer.Name")]
get.user.sales = function(user)
{
 sum(ds[which(user.col == user),"Sales"])
}
#get.user.sales("Tony Sayre")

users = levels(user.col)
user.sum.sales = c()
for(i in 1:length(users))
{
 user.sum.sales[i] = get.user.sales(users[i])
}
min.user.sales = min(user.sum.sales)
max.user.sales = max(user.sum.sales)
user.sum.sales1 = norm1(user.sum.sales)
user.sales = quantile(user.sum.sales1)
user.sales
#     0%        25%        50%        75%       100% 
# 85.720   7364.202  15287.673  25401.008 117124.438  
#       0%        25%        50%        75%       100% 
# 0.00000000 0.06160549 0.12853851 0.21462408 1.00000000 


# 5. MARKET PENETRATION
	#only 4 regions available, so provinces(13) taken instead
get.prod.spread = function(prod)
{
 length(unique(ds[which(prod.col == prod),"Province"]))
}
spread.cnt = c()
for(i in 1:length(prods))
{
 spread.cnt[i] = get.prod.spread(prods[i])
}
min.prod.spread = min(spread.cnt)
max.prod.spread = max(spread.cnt)
spread.cnt1 = norm1(spread.cnt)
prod.spread = quantile(spread.cnt1)
prod.spread
# 0%  25%  50%  75% 100% 
#  1    3    4    6   10 
#  0%  25%  50%  75% 100% 
# 0.0  0.3  0.4  0.6  1.0 


# 6. USER RECENCY
user.order.dates = c()
x = c()
get.user.recency = function(user)
{
 x = ds[which(user.col == user), "Order.Date"]
 for(i in 1:length(x))
 {
  #print(as.Date(toString(x[i]), "%m/%d/%Y"))
    #user.order.dates = c(user.order.dates, as.Date(toString(x[i]), "%m/%d/%Y"))
  user.order.dates[i] = format(as.Date(toString(x[i]), "%m/%d/%Y"))
 }
 #print(user.order.dates)
  #print(sort(format(as.Date(user.order.dates), "%y%m%d")))
 ord = order(format(as.Date(user.order.dates), "%y%m%d"))
 #print(user.order.dates[ord[1]])
   #cat(user.order.dates[1], "\t", tail(user.order.dates, 1), "\n")
  #last.ind = length(user.order.dates)
  #print(user.order.dates[1])
  #print(user.order.dates[last.ind])
   #cat(user.order.dates[1], "\t", user.order.dates[last.ind], "\n")
 #as.double(difftime(Sys.Date(), user.order.dates[ord[1]], units="days"))
 curr.date = format(as.Date("2012-31-12", "%Y-%d-%m"))
 as.numeric(difftime(curr.date, user.order.dates[ord[1]], units="weeks"))
}
#get.user.recency("Don Miller")

user.recency = c()
for(i in 1:length(users))
{
 user.recency[i] = get.user.recency(users[i])
}
user.recency = user.recency[!is.na(user.recency)]
min.user.recency = min(user.recency)
max.user.recency = max(user.recency)
#as.numeric(norm2(get.user.recency("Gene Hale"),min.user.recency,max.user.recency))
user.recency1 = norm1(user.recency)
user.recency = quantile(user.recency1)
user.recency
#       0%       25%       50%       75%      100% 
# 32.00595 162.57143 187.43452 200.14286 208.57143 
# 0%       25%       50%       75%      100% 
# 0.0000000 0.7356302 0.8790749 0.9514547 1.0000000 


# 7. SITE PERFORMANCE
order.dates = ds[,which(names(ds) == "Order.Date")]
order.yrs = c()
for(i in 1:length(order.dates))
{
 order.yrs[i] = format(as.Date(toString(order.dates[i]), "%m/%d/%Y"), "%Y")
}
#order.yrs

get.site.perf = function(yr)
{
 annual.sales = ds[which(order.yrs == yr), "Sales"]
 sum(annual.sales)
}
#get.site.perf(2012)
sales.yr = c()
j = 1
for(i in 2009:2012)
{
 sales.yr[j] = get.site.perf(i)
 j=j+1
}
sales.yr
min.site.perf = min(sales.yr)
max.site.perf = max(sales.yr)
sales.yr1 = norm1(sales.yr)
sales.yr1
site.perf = quantile(sales.yr1)
site.perf
#       0%      25%      50%      75%     100% 
# 37.85296 38.84604 40.02900 42.34280 46.72837
#    0%       25%       50%       75%      100% 
# 0.0000000 0.1093453 0.2561409 0.5248661 1.0000000




# Set universe of discourse
sets_options("universe", seq(from = 0, to = 100, by = 0.1))

# Fuzzy variables
variables =
set(
discount = fuzzy_variable(vl = fuzzy_triangular(corners = c(0,0,22), height = 1),
			    l = fuzzy_trapezoid(corners = c(0,22,33,44)),
			    m = fuzzy_trapezoid(corners = c(33,44,55,66)),
			    h = fuzzy_trapezoid(corners = c(55,66,77,100)),
			    vh = fuzzy_triangular_gset(corners = c(77,100,100), height = 1, universe = seq(from = 77, to = 100, by = 0.1))),
prod.popu = fuzzy_variable(vl = fuzzy_triangular_gset(corners = c(0,0,25), height = 1, universe = seq(from = 0, to = 25, by = 0.1)),
		          l = fuzzy_cone(center = 25, radius = 25),
			    m = fuzzy_cone(center = 50, radius = 25),
			    h = fuzzy_cone(center = 75, radius = 25),
			    vh = fuzzy_triangular_gset(corners = c(75,100,100), height = 1, universe = seq(from = 75, to = 100, by = 0.1))),
category.popu = fuzzy_partition(varnames = c(vl = 0, l = 25, m = 50, h = 75, vh = 100), 
				FUN = fuzzy_cone, radius = 25),
user.sales = fuzzy_variable(unactivated = fuzzy_triangular_gset(corners = c(0, 0, 12.5), universe = seq(from = 0, to = 12.5, by = 0.1)),
                      activated = fuzzy_cone(center = 12.5, radius = 12.5),
			    freq = fuzzy_trapezoid(corners = c(12.5, 50, 62.5, 75)),
			    very.freq = fuzzy_trapezoid(corners = c(62.5, 75, 87.5, 100)),
			    whale = fuzzy_triangular_gset(corners = c(87.5, 100, 100), universe = seq(from = 87.5, to = 100, by = 0.1))),
market.penetration = fuzzy_variable(l = fuzzy_triangular(corners = c(0,0,33)),
			    m = fuzzy_trapezoid(corners = c(0,33,66,100)),
			    h = fuzzy_triangular_gset(corners = c(66,100,100), universe = seq(from = 66, to = 100, by = 0.1))),
user.recency = fuzzy_variable(brand.new = fuzzy_triangular_gset(corners = c(0, 0, 25), universe = seq(from = 0, to = 25, by = 0.1)),
                      new = fuzzy_trapezoid(corners = c(0, 25, 41, 53)),
			    experienced = fuzzy_cone(center = 53, radius = 12),
			    long.term = fuzzy_trapezoid(corners = c(53, 66, 80, 100)),
			    very.long.term = fuzzy_triangular_gset(corners = c(80, 100, 100), universe = seq(from = 80, to = 100, by = 0.1))),
site.perf = fuzzy_variable(terrible = fuzzy_triangular(corners = c(0, 0, 12.5)),
                      bad = fuzzy_trapezoid(corners = c(0, 12.5, 25, 37.5)),
			    ok = fuzzy_trapezoid(corners = c(25, 37.5, 62.5, 75)),
			    good = fuzzy_trapezoid(corners = c(62.5, 75, 87.5, 100)),
			    excellent = fuzzy_triangular_gset(corners = c(87.5, 100, 100), universe = seq(from = 87.5, to = 100, by = 0.1)))
)
#par(mfrow = c(4,2))
layout(matrix(c(1,1,2,3,4,5,6,7),4,2,byrow = TRUE))
plot(variables$discount, main = "Discount")
#par(new = TRUE) # combine plots in a graph instead of a window
plot(variables$prod.popu, main = "Product Popularity")
plot(variables$category.popu, main = "Category Popularity")
plot(variables$user.sales, main = "User Sales")
plot(variables$market.penetration, main = "Market Penetration")
plot(variables$user.recency, main = "User Recency")
plot(variables$site.perf, main = "Site Performance")

# Fuzzy Rule-Base
rules =
set(
fuzzy_rule(prod.popu %is% vl || 
	category.popu %is% vl || 
	user.sales %is% unactivated ||
	user.recency %is% brand.new ||
      site.perf %is% terrible, discount %is% vh),
fuzzy_rule(prod.popu %is% l || 
	category.popu %is% l || 
	user.sales %is% activated ||
	market.penetration %is% h ||
	user.recency %is% new ||
      site.perf %is% bad, discount %is% h),
fuzzy_rule(prod.popu %is% m || 
	category.popu %is% m || 
	user.sales %is% freq ||
	market.penetration %is% m ||
      user.recency %is% experienced ||
	site.perf %is% ok, discount %is% m),
fuzzy_rule(prod.popu %is% h || 
	category.popu %is% h ||
	user.sales %is% very.freq ||
	market.penetration %is% l ||
	user.recency %is% long.term ||
	site.perf %is% good, discount %is% l), 
fuzzy_rule(prod.popu %is% vh || 
	category.popu %is% vh || 
	user.sales %is% whale ||
	user.recency %is% very.long.term ||
	site.perf %is% excellent, discount %is% vl)
)

# Upload FRB to a system
system = fuzzy_system(variables, rules)
print(system)
#plot(system)# plots variables

# Fuzzy Inference - example
inp.prod = "Newell 323"
inp.prod.popu = as.numeric(norm2(get.prod.popu(inp.prod),min.prod.popu,max.prod.popu))
inp.category.popu = as.numeric(norm2(get.prod.category.popu(inp.prod),min.category.popu,max.category.popu))
inp.user = "Gene Hale"
inp.user.sales = as.numeric(norm2(get.user.sales(inp.user),min.user.sales,max.user.sales))
inp.market.penetration = as.numeric(norm2(get.prod.spread(inp.prod),min.prod.spread,max.prod.spread))
inp.user.recency = as.numeric(norm2(get.user.recency(inp.user),min.user.recency,max.user.recency))
inp.site.perf = as.numeric(norm2(get.site.perf(2012),min.site.perf,max.site.perf))
cat("\n\nprod.popu ", inp.prod.popu, 
"\ncategory.popu ", inp.category.popu, 
"\nuser.sales ", inp.user.sales, 
"\nmarket.penetration ", inp.market.penetration,
"\nuser.recency ", inp.user.recency, 
"\nsite.perf ", inp.site.perf)
fi = fuzzy_inference(system, list(prod.popu = inp.prod.popu, 
category.popu = inp.category.popu, 
user.sales = inp.user.sales, 
market.penetration = inp.market.penetration,
user.recency = inp.user.recency, 
site.perf = inp.site.perf))
plot(fi)# plot resulting (ouput - discount) fuzzy set
gset_defuzzify(fi, "centroid")
sets_options("universe", NULL)# reset universe

# Test
prod.col = test.ds[,which(names(test.ds) == "Product.Name")]
user.col = test.ds[,which(names(test.ds) == "Customer.Name")]
disc.col = test.ds[,which(names(test.ds) == "Discount")]
disc.col = ds[,which(names(ds) == "Discount")]
disc=c()
err=0
#n=100
#for(i in n:(2*n))
for(i in 1:100)
#for(i in 1:length(test.ds))
{
inp.prod = prod.col[i]
inp.prod.popu = as.numeric(norm2(get.prod.popu(inp.prod),min.prod.popu,max.prod.popu))
inp.category.popu = as.numeric(norm2(get.prod.category.popu(inp.prod),min.category.popu,max.category.popu))
inp.user = user.col[i]
inp.user.sales = as.numeric(norm2(get.user.sales(inp.user),min.user.sales,max.user.sales))
inp.market.penetration = as.numeric(norm2(get.prod.spread(inp.prod),min.prod.spread,max.prod.spread))
inp.user.recency = as.numeric(norm2(get.user.recency(inp.user),min.user.recency,max.user.recency))
inp.site.perf = as.numeric(norm2(get.site.perf(2012),min.site.perf,max.site.perf))
cat("\n\nprod.popu ", inp.prod.popu, 
"\ncategory.popu ", inp.category.popu, 
"\nuser.sales ", inp.user.sales, 
"\nmarket.penetration ", inp.market.penetration,
"\nuser.recency ", inp.user.recency, 
"\nsite.perf ", inp.site.perf)
fi = fuzzy_inference(system, list(prod.popu = inp.prod.popu, 
category.popu = inp.category.popu, 
user.sales = inp.user.sales, 
market.penetration = inp.market.penetration,
user.recency = inp.user.recency, 
site.perf = inp.site.perf))
disc[i] = gset_defuzzify(fi, "centroid")
 #if(disc[i]>20 + mean(disc.col))# 0.05295188
 #{
  disc[i] = disc[i]-20
 #}
 if(disc[i]<0)
 {
  disc[i] = 0
 }
 disc.col[i] = disc.col[i]*100
 cat("\nestimate = ", disc[i],"\tactual = ", disc.col[i],"\n")
 #err = err+(disc[i]-disc.col[i])/(disc.col[i]+0.0001)
 err = err+min(disc[i], disc.col[i])/(max(disc[i], disc.col[i])+0.0001)
}
percent.err = round(err,2)
cat("\naccuracy = ", percent.err, "%")
accuracy(disc,disc.col)
var(disc)# 3.446451