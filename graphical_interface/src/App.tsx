import { useState } from 'react'
import './App.css'
import Navbar from './components/Navbar/Navbar'
import { BrowserRouter, Route, Routes } from 'react-router-dom';
import SatSolverContent from './components/SatSolverContent/SatSolverContent';
import ResolutionContent from './components/ResolutionContent/ResolutionContent';

function App() {
  const [openBasic, setOpenBasic] = useState<boolean>(false); 
  const links = [
    { name: 'Resolution', href: '/resolution' },
    { name: 'Davis-Putnam', href: '/davis-putnam' },
    { name: 'Home', href: '/' },
  ];

  return (
    <BrowserRouter>
      <div className="container w-100 p-0 m-0 mw-100 d-flex flex-col" style={{flexWrap:"wrap", alignContent: "flex-start"}}>
        <div className="row w-100 p-0 m-0">
          <div className="col p-0">
            <Navbar openBasic={openBasic} setOpenBasic={setOpenBasic} links={links} brandName="Project1-KRR" />
          </div>
        </div>
        <div className="row w-100 p-0 m-0 h-100">
          <Routes>
            <Route path="/" element={<div>Home Page</div>} />
            <Route path="/davis-putnam" element={<SatSolverContent/>} />
            <Route path="/resolution" element={<ResolutionContent/>} />
          </Routes>
        </div>
       
      </div>
    </BrowserRouter>
  )
}

export default App
