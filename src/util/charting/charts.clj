(ns util.charting.charts
  (:import (org.jfree.chart ChartFactory ChartPanel JFreeChart))
  (:import (org.jfree.chart.axis DateAxis))
  (:import (org.jfree.chart.plot XYPlot))
  (:import (org.jfree.chart.renderer.xy XYItemRenderer XYLineAndShapeRenderer ))
  (:import (org.jfree.data.time Month Year TimeSeries TimeSeriesCollection ))
  (:import (org.jfree.data.xy XYDataset))
  (:import (org.jfree.ui ApplicationFrame RectangleInsets RefineryUtilities))
  (:import (javax.swing JFrame JButton JOptionPane))
  (:import (java.awt.event ActionListener))
)


(defn show [chart]
  (let [cp (ChartPanel. chart) frame (JFrame. "Plot")]
    (.setContentPane frame cp)
    (.pack frame)
    (.setVisible frame true)))

(defn plot-timeseries-yearwise [data xtitle ytitle]
    (let [ts (TimeSeries. "") yrs (keys data) tc (TimeSeriesCollection.)]
      (doseq [ky yrs] (.add ts (Year. (Integer. ky)) (data ky)))
        (.addSeries tc ts)
        (show (ChartFactory/createTimeSeriesChart "" xtitle ytitle tc true true false))))

(plot-timeseries-yearwise {"2001" 2.5 2002 3.3 2003 1.4 2004 3.5 2006 3.7} , "Year", "Money")
