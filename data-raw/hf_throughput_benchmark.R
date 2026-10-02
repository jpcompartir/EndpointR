# Throughput of Hugging Face dedicated endpoints, measured on 1 October 2026
# from a laptop in the UK against A100 endpoints in AWS us-east-1.
# texts_per_sec is the end-to-end rate seen by the client.

hf_throughput_benchmark <- utils::read.csv(text = "
model,engine,hardware,method,texts_per_request,concurrent_requests,sorted,n_texts,texts_per_sec
modernbert-spam,toolkit,A100,hf_classify_df default,1,1,FALSE,300,8.1
modernbert-spam,toolkit,A100,hf_classify_df,1,50,FALSE,1000,45.6
modernbert-spam,toolkit,A100,batched without pipeline batch_size,64,4,FALSE,1000,53.6
modernbert-spam,toolkit,A100,batched with pipeline batch_size,64,8,FALSE,100000,367
modernbert-spam,toolkit,A100,batched with pipeline batch_size,64,8,TRUE,100000,436
modernbert-spam,toolkit,A100,token budget batches up to 512,512,8,TRUE,100000,354
modernbert-spam,tei,A100,batched,128,32,FALSE,100000,4305
modernbert-spam,tei,A100,batched,64,16,FALSE,100000,4094
bge-m3,tei,A100,hf_embed_df default,1,1,FALSE,500,9
bge-m3,tei,A100,earlier internal tool default,1,5,FALSE,1000,28
bge-m3,tei,A100,one text per request,1,32,FALSE,20000,187
bge-m3,tei,A100,batched,8,16,FALSE,20000,856
bge-m3,tei,A100,batched,32,8,FALSE,20000,1596
bge-m3,tei,A100,batched,32,8,TRUE,20000,1474
bge-m3,tei,A100,batched,32,32,TRUE,50000,2083
bge-m3,tei,A100,batched,32,32,FALSE,20000,2245
modernbert-spam,tei,A100,full run of 2.19 million messages,128,32,FALSE,2189478,2799
bge-m3,tei,A100,full run of 2.19 million messages,32,64,FALSE,2189478,1819
", strip.white = TRUE)

hf_throughput_benchmark <- tibble::as_tibble(hf_throughput_benchmark)

usethis::use_data(hf_throughput_benchmark, overwrite = TRUE)
