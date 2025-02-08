import React, { useEffect, useState } from 'react'
import { CaseComponent } from '../../models/CaseComponent';

const ResolutionContent:React.FC = () => {
    const [cases, setCases] = useState<Array<CaseComponent>>([]);
    const [loading, setLoading] = useState<boolean>(true);
    const [error, setError] = useState<string | null>(null);
    const [selectedPath, setSelectedPath] = useState<string | null>(null);
    const [selectedContent, setSelectedContent] = useState<string | null>(null);
    const [output, setOutput] = useState<string | null>(null);
    const [isInput, setIsInput] = useState<boolean>(false);

    const delay = (ms: number) => new Promise(resolve => setTimeout(resolve, ms));

    const handleButtonClick = (satCase: CaseComponent) => {
        setSelectedPath(satCase.absolutePath);
        setSelectedContent(satCase.content);  
    };

    const handleSubmitQuery = async () => {
        if (!selectedPath) {
            setOutput('Please select a case first.');
            return;
        }
        try {
            const response = isInput ?
            await fetch('http://localhost:3001/solve-resolution-input', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({ pathToSolve: selectedPath, content: selectedContent }),  
            })
            :
            await fetch('http://localhost:3001/solve-resolution', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({ pathToSolve: selectedPath }),  
            });
            if (!response.ok) {
                throw new Error('Failed to submit query.');
            }
            const data = await response.json();
            setOutput(data.output || 'Unknown response from server.');
        } catch (err:any) {
            setOutput(err.message || 'Error submitting query.');
        }
    };

    useEffect(() => {
        const fetchData = async () => {
        try {
            await delay(2000);
            const response = await fetch('http://localhost:3001/cases-resolution');
            if (!response.ok) {
            throw new Error('Failed to fetch cases');
            }
            const data = await response.json();
            setCases(data);
            setLoading(false);
        } catch (err : any) {
            setError(err.message);
            setLoading(false);
        }
        };
        fetchData();
    }, []);

    if (error) {
        return <div>Error: {error}</div>;
    }

    if (loading) {
        return<div className="col p-0 d-flex justify-content-center align-items-center">
            <div className="spinner-border text-primary" role="status">
                <span className="visually-hidden">Loading...</span>
            </div>
        </div>
    }

    return (
        <div className="col p-0 d-flex justify-content-center">
            {loading 
            ?
            <div className="spinner-border text-primary" role="status">
                <span className="visually-hidden">Loading...</span>
            </div> 
            : 
            <div className="card w-100" data-mdb-theme="dark" style={{borderRadius: "0"}}>
                <div className="card-body p-0">
                    <div className='d-flex flex-row flex-wrap justify-content-around mt-3 mb-3 w-100'>
                        {cases.map((satCase, index) => (
                            <button key={index} type="button" className={`m-2 btn ${index % 2 ? "btn-primary" : "btn-secondary"}`} onClick={
                                satCase.name.toLowerCase() === "input" ?
                                () => { setIsInput(true); handleButtonClick(satCase); }    
                                : () => { setIsInput(false); handleButtonClick(satCase); }
                            } data-mdb-ripple-init>
                                {satCase.name}        
                            </button>
                        ))}
                    </div>
                    <div className='d-flex flex-row justify-content-around mt-5 w-100'>
                        <div className="form-outline mw-50 m-2" data-mdb-input-init>
                            <textarea 
                                className="form-control" 
                                id="inputContentArea"
                                rows={4} 
                                style={{ border: "1px solid #ced4da", borderRadius: "4px" }} 
                                value={selectedContent || ''} 
                                readOnly={!isInput} 
                                onChange={(e) => setSelectedContent(e.target.value)}>
                            </textarea>
                        </div>
                        <div className="form-outline mw-50 m-2" data-mdb-input-init>
                            <textarea 
                                className="form-control" 
                                id="inputContentArea"
                                rows={4} 
                                style={{ border: "1px solid #ced4da", borderRadius: "4px" }} 
                                value={output || ''} 
                                readOnly>
                            </textarea>
                        </div>
                    </div>
                    <div className='d-flex justify-content-around mt-3'>
                        <button type="button" className="btn btn-info" onClick={handleSubmitQuery} data-mdb-ripple-init>Submit Query</button>
                    </div>
                </div>
            </div>}
        </div>
    );
}

export default ResolutionContent