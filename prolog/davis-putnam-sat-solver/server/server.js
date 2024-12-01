let fetch;

import('node-fetch').then((module) => {
    fetch = module.default;  

    const express = require('express');
    const cors = require('cors');
    const fs = require('fs');
    const path = require('path');

    const app = express();
    const PORT = 3001;

    const casesFolder = path.join(__dirname, '../cases');

    app.use(express.json());
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

    app.post('/solve-davis-putnam', async (req, res) => {
        const { pathToSolve, strategy } = req.body; 
        if (!pathToSolve || strategy === undefined) {
            return res.status(400).json({ error: 'Path or strategy not provided' });
        }
        try {
            const response = await fetch('http://localhost:3002/solve', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({ path: pathToSolve, strategy }),
            });
            if (!response.ok) {
                throw new Error('Failed to communicate with Prolog server');
            }
            const data = await response.json();
            res.json(data);  
        } catch (err) {
            console.error(err);
            res.status(500).json({ error: 'Error communicating with Prolog server' });
        }
    });

    app.listen(PORT, () => {
        console.log(`Server running at http://localhost:${PORT}`);
    });
}).catch((err) => {
    console.error('Error loading node-fetch:', err);
});
