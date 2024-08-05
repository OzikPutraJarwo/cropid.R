#' Analisis Varians (ANOVA) dan Uji Lanjut (Post Hoc Test)
#'
#' @param excel_j Jalur ke file data excel.
#' @param excel_k Tipe kolom data excel.
#' @param sheet_n Nama sheet yang akan digunakan.
#' @param sheet_k Nama kolom hasil dalam sheet yang digunakan.
#' @param anova_r Tipe ANOVA ("RAK" atau "RAL").
#' @param anova_p Nama kolom perlakuan.
#' @param anova_u Nama kolom ulangan.
#' @param posthoc Tipe uji lanjut ("BNT", "BNJ", atau "DMRT").
#' @return Hasil ANOVA dan Uji Lanjut.
#' @export
avs <- function(excel_j, excel_k, sheet_n, sheet_k, anova_r, anova_p, anova_u, posthoc) {

  if (anova_r == "RAK"){
    anova_form <- paste("~", anova_u, "+", anova_p)
  } else if (anova_r == "RAL") {
    anova_form <- paste("~", anova_p)
  }
  sheet <- sheet_n
  col_name <- sheet_k
  data <- read_excel(excel_j, sheet = sheet, col_types = excel_k)
  anova_formula <- as.formula(paste(col_name, anova_form))
  anova <- aov(anova_formula, data = data)
  anova_summary <- summary(anova)
  
  anova_df <- as.data.frame(anova_summary[[1]])
  anova_df <- rownames_to_column(anova_df, var = "Term")
  anova_df$Term <- gsub("Residuals", "Galat", anova_df$Term)
  colnames(anova_df)[colnames(anova_df) == "Df"] <- "db"
  colnames(anova_df)[colnames(anova_df) == "Sum Sq"] <- "JK"
  colnames(anova_df)[colnames(anova_df) == "Mean Sq"] <- "KT"
  colnames(anova_df)[colnames(anova_df) == "F value"] <- "F-hit"
  
  if (anova_r == "RAK"){
    f_table <- qf(0.95, anova_df$db[1:2], anova_df$db[3])
  } else if (anova_r == "RAL") {
    f_table <- qf(0.95, anova_df$db[1], anova_df$db[2])
  }
  anova_df$`F-tab` <- c(f_table, NA)
  
  colnames(anova_df)[colnames(anova_df) == "Pr(>F)"] <- "P-val"
  anova_df <- anova_df %>% relocate(`F-tab`, .before = `P-val`)
  anova_df <- column_to_rownames(anova_df, var = "Term")

  if (anova_r == "RAK"){
    db_Total <- (anova_df$db[1] + 1) * (anova_df$db[2] + 1) - 1
    db_JK <- sum(anova_df$JK[1:3])
  } else if (anova_r == "RAL") {
    db_Total <- sum(anova_df$db[1:2])
    db_JK <- sum(anova_df$JK[1:2])
  }

  total_row <- setNames(data.frame(
    db = db_Total,
    JK = db_JK,
    KT = NA,
    `F-hit` = NA,
    `F-tab` = NA,
    `P-val` = NA
  ), names(anova_df))
  
  anova_df <- rbind(anova_df, total_row)
  rownames(anova_df)[nrow(anova_df)] <- "Total"

  anova_df <- anova_df %>%
  mutate(across(c(db, JK, KT, `F-hit`, `F-tab`, `P-val`), ~ round(., 2)))

  if (anova_r == "RAK"){
    sigma_KTG <- anova_df$KT[3]
  } else if (anova_r == "RAL") {
    sigma_KTG <- anova_df$KT[2]
  }
  mu_KTG <- mean(data[[col_name]], na.rm = TRUE)
  cv <- round((sqrt(sigma_KTG) / mu_KTG * 100), 0)

  signif <- ifelse(
    anova_df$`P-val` <= 0.01, green("** (sn)"),
    ifelse(anova_df$`P-val` <= 0.05, yellow("* (n)"), red("tn"))
  )
  signif[is.na(anova_df$`P-val`)] <- NA

  df_error <- df.residual(anova)
  MSE <- sum(anova$residuals^2) / df_error
  n_groups <- length(unique(data[[anova_p]]))
  r_groups <- length(unique(data[[anova_u]]))
  emmeans_result <- emmeans(anova, as.formula(paste("~", anova_p)))

  add_indent <- function(groups) {
    unique_groups <- unique(groups)
    indented_groups <- character(length(groups))
    for (i in seq_along(groups)) {
      indent_level <- match(groups[i], unique_groups)
      indented_groups[i] <- paste0(strrep(" ", indent_level), groups[i])
    }
    return(indented_groups)
  }

  if (grepl("BNT", posthoc)){
    lsd_cld <- cld(emmeans_result, adjust = "none", Letters = letters, alpha = 0.05)
    LSD_value <- round(qt(1 - 0.05/2, df_error) * (sqrt(2 * MSE / r_groups)), 2)
  }

  if (grepl("BNJ", posthoc)){
    tukey_result <- multcomp::cld(emmeans_result, Letters = letters)
    ordered_results <- tukey_result[order(tukey_result$emmean), ]
    tukey_value <- round(qtukey(0.95, n_groups, df_error) * (sqrt(MSE / r_groups)), 2)
  }

  if (grepl("DMRT", posthoc))   {
    dmrt_result <- agricolae::duncan.test(anova, anova_p, group = TRUE)
    dmrt_groups <- dmrt_result$groups
    colnames(dmrt_groups) <- c("Rerata", "Notasi")
    dmrt_groups$Notasi <- add_indent(dmrt_groups$Notasi)
    DMRT_value <- round(qt(1 - 0.05/2, df_error) * (sqrt(MSE / r_groups)), 2)
  }

  cat(bold(yellow("=============================================\n\n")))
  cat(bold(blue("Nama Sheet :", sheet)))
  cat("\n")
  cat(bold(blue("Kolom      :", col_name)))
  cat("\n\n")
  cat(bold("ANOVA", anova_r))
  cat("\n")
  print(anova_df)
  cat("\n")
  cat(bold("Koefisien Korelasi:"), paste0(cv, "%"))
  cat("\n\n")
  cat(bold("Signifikansi"))
  cat("\n")
  if (anova_r == "RAK"){
    cat("-", anova_u, ":", signif[1])
    cat("\n")
    cat("-", anova_p, ":", signif[2])
  } else if (anova_r == "RAL") {
    cat("-", anova_p, ":", signif[1])
  }
  cat("\n")
  if (grepl("BNT", posthoc)){
    cat("\n")
    cat(bold("BNT :"), LSD_value)
    cat("\n\n")
    cat(sprintf("%-10s %-10s %-10s\n",
                anova_p, "Rerata", "Notasi"))
    for (i in 1:nrow(lsd_cld)) {
      cat(sprintf("%-10s %-10s %-10s\n",
                  lsd_cld[[anova_p]][i],
                  round(lsd_cld$emmean[i], 2),
                  lsd_cld$.group[i]))
    }
  }
  if (grepl("BNJ", posthoc)){
    cat("\n")
    cat(bold("BNJ :"), tukey_value)
    cat("\n\n")
    cat(sprintf("%-10s %-10s %-10s\n",
                anova_p, "Rerata", "Notasi"))
    for (i in 1:nrow(ordered_results)) {
      cat(sprintf("%-10s %-10s %-10s\n",
                  ordered_results[[anova_p]][i],
                  round(ordered_results$emmean[i], 2),
                  ordered_results$.group[i]))
    }
  }
  if (grepl("DMRT", posthoc)) {
    cat("\n")
    cat(bold("DMRT :"), DMRT_value)
    cat("\n\n")
    cat(sprintf("%-10s %-10s %-10s\n", "Perlakuan", "Rerata", "Notasi"))  
    for (i in seq_len(nrow(dmrt_groups))) {
      cat(sprintf("%-10s %-10.2f %-10s\n", 
                  rownames(dmrt_groups)[i], 
                  dmrt_groups[i, "Rerata"], 
                  dmrt_groups[i, "Notasi"]))
    }
  }
  cat(bold(yellow("\n=============================================\n\n")))
}
