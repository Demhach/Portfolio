/* 

	Cleaning Data in SQL Queries

*/
SELECT
	*
FROM 
	PortfolioProject..NashVilleHousing
---------------------------------------------------------------------------------------------

-- Standarize Date Format
SELECT
	SaleDate,
	CONVERT (DATE,SaleDate)
FROM 
	PortfolioProject..NashVilleHousing

ALTER TABLE 
	NashVilleHousing
ALTER COLUMN
	SaleDate DATE

---------------------------------------------------------------------------------------------

--Populate Property Address data
SELECT 
	*
FROM	
	PortfolioProject..NashVilleHousing
WHERE
	PropertyAddress IS NULL
ORDER BY 
	ParcelID

-- defining the range of data to update
SELECT 
	a.ParcelID,
	a.PropertyAddress,
	b.ParcelID,
	b.PropertyAddress,
	ISNULL(a.PropertyAddress,b.PropertyAddress)
FROM	
	PortfolioProject..NashVilleHousing AS a
JOIN 
	PortfolioProject..NashVilleHousing AS b
	ON a.ParcelID = b.ParcelID
	AND a.[UniqueID ] <> b.[UniqueID ]
WHERE 
	a.PropertyAddress IS NULL

--updating the table
UPDATE
	a
SET 
	PropertyAddress = ISNULL(a.PropertyAddress,b.PropertyAddress)
FROM	
	PortfolioProject..NashVilleHousing AS a
JOIN 
	PortfolioProject..NashVilleHousing AS b
	ON a.ParcelID = b.ParcelID
	AND a.[UniqueID ] <> b.[UniqueID ]
WHERE 
	a.PropertyAddress IS NULL

------------------------------------------------------------------------------------------------------------
--Breaking out Address into Individual Columns (Address, City, State)

--splitting the propertyaddress into p_address and p_city
SELECT 
	PropertyAddress,
	P_Address, -- new property address
	P_City -- new property city
FROM	
	PortfolioProject..NashVilleHousing
--WHERE
	--PropertyAddress IS NULL
--ORDER BY 
	--ParcelID

SELECT
	SUBSTRING(PropertyAddress, 1, CHARINDEX(',', PropertyAddress)-1) AS P_Address,
	SUBSTRING(PropertyAddress, CHARINDEX(',', PropertyAddress)+1, LEN(PropertyAddress)) AS P_City
FROM	
	PortfolioProject..NashVilleHousing


ALTER TABLE
	NashVilleHousing
DROP COLUMN
	P_Address,
	P_City

ALTER TABLE
	NashVilleHousing
ADD
	P_Address NVARCHAR(255), 
	P_City NVARCHAR(255)

UPDATE
	NashVilleHousing
SET
	P_Address = SUBSTRING(PropertyAddress, 1, CHARINDEX(',', PropertyAddress)-1),
	P_City = SUBSTRING(PropertyAddress, CHARINDEX(',', PropertyAddress)+1, LEN(PropertyAddress))



--splitting the owneraddress into o_address, o_city, o_state

SELECT 
	OwnerAddress,
	O_Address, --new owner address
	O_City, -- new owner city
	O_state -- new owner state
FROM	
	PortfolioProject..NashVilleHousing
--WHERE
	--PropertyAddress IS NULL
--ORDER BY 
	--ParcelID

SELECT
	PARSENAME(REPLACE(OwnerAddress, ',', '.'), 1) AS State,
	PARSENAME(REPLACE(OwnerAddress, ',', '.'), 2) AS City,
	PARSENAME(REPLACE(OwnerAddress, ',', '.'), 3) AS Address

FROM	
	PortfolioProject..NashVilleHousing


ALTER TABLE
	NashVilleHousing
DROP COLUMN
	O_Address,
	O_City,
	O_state

ALTER TABLE
	NashVilleHousing
ADD
	O_Address NVARCHAR(255), 
	O_City NVARCHAR(255),
	O_state NVARCHAR(255)

UPDATE
	NashVilleHousing
SET
	O_state = PARSENAME(REPLACE(OwnerAddress, ',', '.'), 1),
	O_City = PARSENAME(REPLACE(OwnerAddress, ',', '.'), 2),
	O_Address = PARSENAME(REPLACE(OwnerAddress, ',', '.'), 3)
-----------------------------------------------------------------------------------------------------------------------
-- Change Y and N to Yes and No in "Sold as Vacant" field


---checking the distinct values in the field to find out the error
SELECT
	DISTINCT SoldAsVacant,
	COUNT(SoldAsVacant) AS count
FROM 
	PortfolioProject..NashVilleHousing
GROUP BY
	SoldAsVacant
---cleaning process
UPDATE
	NashVilleHousing
SET
	SoldAsVacant = 
		CASE
			WHEN SoldAsVacant = 'N' THEN 'No'
			WHEN SoldAsVacant = 'Y' THEN 'Yes'
			ELSE SoldAsVacant
		END
-----------------------------------------------------------------------------------------------------------------------------------------
--Remove Duplicates

	--Identifying duplicates
WITH RowNumCTE AS (
SELECT 
	*,
	ROW_NUMBER() OVER (PARTITION BY ParcelID,
									PropertyAddress,
									SalePrice,
									SaleDate,
									LegalReference
									ORDER BY
										UniqueID) AS rownumber
FROM 
	PortfolioProject..NashVilleHousing
)

SELECT
	*
FROM
	RowNumCTE
WHERE
	rownumber> 1
ORDER BY 
	4

	--deleting the duplicates
WITH RowNumCTE AS (
SELECT 
	*,
	ROW_NUMBER() OVER (PARTITION BY ParcelID,
									PropertyAddress,
									SalePrice,
									SaleDate,
									LegalReference
									ORDER BY
										UniqueID) AS rownumber
FROM 
	PortfolioProject..NashVilleHousing
)

DELETE
FROM
	RowNumCTE
WHERE
	rownumber> 1

-------------------------------------------------------------------------------------------------------------------------------
--Delete Unused Columns
SELECT 
	*
FROM
	PortfolioProject..NashVilleHousing

ALTER TABLE
	PortfolioProject..NashVilleHousing
DROP COLUMN 
	PropertyAddress,
	OwnerAddress,
	TaxDistrict