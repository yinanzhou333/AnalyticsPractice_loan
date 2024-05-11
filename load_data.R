# Load tha dataset
df <- read.csv("Loan_default.csv",header = TRUE)
head(df)
str(df)
df <- df[, -1]  # Removes the first column
char_cols <- sapply(df, is.character)
for (col in names(df)[char_cols]) {
  df[[paste0(col, "_factor")]] <- factor(df[[col]], levels = unique(df[[col]]))
  df[[paste0(col, "_integer")]] <- as.integer(df[[paste0(col, "_factor")]])
}
df <- df[sapply(df, is.integer) | sapply(df, is.numeric)]
#df$Default <- factor(df$Default, levels = c("0", "1"))

# Min-Max scaling function
min_max_scale <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Apply Min-Max scaling to numeric columns in df
numeric_cols <- sapply(df, is.numeric)
df <- as.data.frame(lapply(df[numeric_cols], min_max_scale))


str(df)  
summary(df)
colnames(df)