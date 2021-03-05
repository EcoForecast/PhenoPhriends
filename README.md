# PhenoPhriends
2021 BU EE585 course project: EFI/NEON phenology forecast


Name: Mira Kelly-Fair
Email: mirakf@bu.edu
Phone number: (415)312-4236

Name: Charlotte Malmborg
Email: malmborg@bu.edu
Phone number: 802-310-4177

Name: Samuel Agate 
Email: sagate@bu.edu
Phone Number: 510-559-0234

Name: Devin Hubbard
Email: dhubbard@bu.edu
Phone Number: 978-460-2546

```
MAILTO=malmborg@bu.edu
15 18 * * * /home/scratch/dietze_lab/NOMADS/get_sref.sh
```

```
  subset <- MODISTools::mt_subset(product = "MOD13Q1",
                                band = "250m_16_days_EVI",
                                lat=46.0827,
                                lon=-89.9792,
                                km_lr = 1,
                                km_ab = 1,
                                site_name = "WillowCreek")
 * * * * 2 /home/scratch/hubbard_lab/MODIS/MOD13Q1   
