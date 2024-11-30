import React, { useEffect, useState } from 'react'
import { SatSolverComponent } from '../../models/SatSolverComponent';

const SatSolverContent:React.FC = () => {
  const [cases, setCases] = useState<Array<SatSolverComponent>>([]);
  const [loading, setLoading] = useState<boolean>(true);
  const [error, setError] = useState<string | null>(null);


  const delay = (ms: number) => new Promise(resolve => setTimeout(resolve, ms));

  useEffect(() => {
    const fetchData = async () => {
      try {
        await delay(2000);
        const response = await fetch('http://localhost:3001/cases');
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
            <div className="card-body">
                <div>
                    {cases.map((satCase, index) => (
                        <button type="button" className={``} data-mdb-ripple-init>

                        </button>
                    ))}
                </div>
                <ul>
                    {cases.map((satCase) => (
                    <li key={satCase.name}>
                        <h2>{satCase.name}</h2>
                        <p><strong>Absolute Path:</strong> {satCase.absolutePath}</p>
                        <pre><strong>Content:</strong> {satCase.content}</pre>
                    </li>
                    ))}
                </ul>
            </div>
        </div>}
    </div>
  );
}

export default SatSolverContent