SELECT
c.id,
c.namn,
GREATEST(
  0,
  COALESCE(SUM(
    CASE
    WHEN s.sald = 1
    AND s.sald_datum >= DATE_SUB(CURDATE(), INTERVAL 1 YEAR)
    THEN s.sald_pris
    ELSE 0
    END
  ),0)
  -
    COALESCE(p.total_paid,0)
) AS available_balance
FROM clients c
LEFT JOIN stock s ON c.id = s.id_kund
LEFT JOIN (
  SELECT id_kund, SUM(belopp) AS total_paid
  FROM payments
  WHERE utbetald = 1
  GROUP BY id_kund
) p ON c.id = p.id_kund
GROUP BY c.id, c.namn;