const express = require('express');
const cors = require('cors');
const fs = require('fs');
const path = require('path');

const app = express();
const PORT = 3001;

const casesFolder = path.join(__dirname, '../cases'); 

app.use(cors());

app.get('/cases', (req, res) => {
    try {
        const files = fs.readdirSync(casesFolder).filter(file => file.endsWith('.txt'));
        const result = files.map(file => {
            const absolutePath = path.join(casesFolder, file).replace(/\\/g, '/'); 
            const content = fs.readFileSync(path.join(casesFolder, file), 'utf-8').replace(/\r\n/g, ''); 
            return {
                name: path.basename(file, '.txt'), 
                absolutePath,
                content,
            };
        });
        res.json(result);
    } catch (error) {
        res.status(500).send(`Error reading cases folder: ${error.message}`);
    }
});

app.listen(PORT, () => {
    console.log(`Server running at http://localhost:${PORT}`);
});