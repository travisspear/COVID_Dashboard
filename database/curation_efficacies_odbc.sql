with om as (
select distinct
st.PubMedId as SourceId, 
st.MedAwareId,
ob.Id as ObservationId,
obtg.Id as ObservationTreatmentGroupId,
tp.IsBaseline as Time_IsBaseline,
tp.Duration as Time_Interval,
--Outcomes
eff.Mean as Measurement_Mean, 
eff.Median as Measurement_Median,
eff.ChangeInMean as Measurement_ChangeInMean,
eff.ChangeInMedian as Measurement_ChangeInMedian,
eff.NumberOfPatients as Measurement_Number,
eff.PercentOfPatients as Measurement_Percent,
eff.NumberOfEvents AS Measurement_nEvents,
eff.PercentOfEvents AS Measurement_percEvents,
eff.VarianceSd as Measurement_SD, 
eff.VarianceSe AS Measurement_SE,
eff.[VarianceCi99Upper],
eff.[VarianceCi99Lower],
eff.[VarianceCi99Value],
eff.[VarianceCi95Upper],
eff.[VarianceCi95Lower],
eff.[VarianceCi95Value],
eff.[VarianceCi90Upper],
eff.[VarianceCi90Lower],
eff.[VarianceCi90Value],
eff.[VarianceRangeUpper],
eff.[VarianceRangeLower],
eff.[VarianceIqrUpper],
eff.[VarianceIqrLower],
eff.[VarianceIqrValue],
eff.EventCriteriaScore as Measurement_EventCriteriaScore,
eff.[EventCriteriaRangeUpper] as Measurement_EventCriteriaScoreUpper,
eff.[EventCriteriaRangeLower] as Measurement_EventCriteriaScoreLower,
sta.StudyArea,
aemode.Name as Measurement_Mode,
stat.Name as [State],
omname.Name AS Measurement_Name,
evcriop.Name as Measurement_EventCriteriaOperator,
evcrinum.Name as Measurement_EventCriteriaUnitNumerator,
evcrinum.Name as Measurement_EventCriteriaUnitDenominator,
valtype.Name AS Measurement_ValueType,
vartype.Name AS Measurement_VarianceType,
biosource.Name AS Measurement_Source,
effunitnum.Name AS Measurement_UnitNumerator,
effunitden.Name as Measurement_UnitDenominator,
efftype.Name AS Measurement_Type,
tpunit.Name AS Time_Unit


from dbo.Study st
left join dbo.studyassignment sta on sta.studyid=st.id
left join dbo.studyassignmentstate stat on stat.Id=sta.StudyAssignmentStateid
left join dbo.StudyUser stu on stu.Id=sta.StudyUserId
left join dbo.observation ob on ob.StudyAssignmentId=sta.id
left join dbo.observationtreatmentgroup obtg on obtg.observationid=ob.id
left join dbo.Timepoint tp on tp.observationtreatmentgroupid=obtg.Id

-- Treatments
left join dbo.observationtgtreatment obtx on obtx.observationtreatmentgroupid=obtg.id
left join dbo.treatment tx on tx.timepointid=tp.id
left join dbo.treatmenttype txname on txname.id=tx.treatmenttypeid
left join dbo.QualifierType txq on tx.[QualifierTypeId]=txq.Id
left join dbo.Measurementunittype tpunit on tpunit.Id = tp.DurationMeasurementUnitTypeId

-- Outcomes
left join dbo.Efficacy eff on eff.TimepointId = tp.Id
left join dbo.OutcomeMeasureType omname on omname.Id=eff.OutcomeMeasureTypeId
left join dbo.EfficacyType efftype on efftype.Id=eff.EfficacyTypeId
left join dbo.BiomarkerSourceType biosource on biosource.Id=eff.BiomarkerSourceTypeId
left join dbo.MeasurementUnitType effunitnum on effunitnum.Id=eff.[OutcomeMeasureMeasurementUnitTypeId]
left join dbo.MeasurementUnitType effunitden on effunitden.Id=eff.[OutcomeMeasureMeasurementUnitTypeId]
left join dbo.VarianceType vartype on vartype.Id=eff.VarianceTypeId
left join dbo.EfficacyValueType valtype on valtype.Id=eff.EfficacyValueTypeId
left join dbo.EqualityOperatorType evcriop on evcriop.Id = eff.[EqualityOperatorTypeId]
left join dbo.MeasurementUnitType evcrinum on evcrinum.Id=eff.[EventCriteriaMeasurementUnitTypeId]
left join dbo.MeasurementUnitType evcriden on evcriden.Id=eff.[EventCriteriaDenominatorMeasurementUnitTypeId]
left join dbo.adverseeventmodetype aemode on aemode.Id=eff.AdverseEventModeTypeId

where sta.StudyArea like '%coronavirus%' and efftype.Name = 'Outcome Measure' and stat.Name = 'Finalized'),

biomarker as (
select distinct
st.PubMedId as SourceId, 
st.MedAwareId,
ob.Id as ObservationId,
obtg.Id as ObservationTreatmentGroupId,
tp.IsBaseline as Time_IsBaseline,
tp.Duration as Time_Interval,

--Outcomes
eff.Mean as Measurement_Mean, 
eff.Median as Measurement_Median,
eff.ChangeInMean as Measurement_ChangeInMean,
eff.ChangeInMedian as Measurement_ChangeInMedian,
eff.NumberOfPatients as Measurement_Number,
eff.PercentOfPatients as Measurement_Percent,
eff.NumberOfEvents AS Measurement_nEvents,
eff.PercentOfEvents AS Measurement_percEvents,
eff.VarianceSd as Measurement_SD, 
eff.VarianceSe AS Measurement_SE,
eff.[VarianceCi99Upper],
eff.[VarianceCi99Lower],
eff.[VarianceCi99Value],
eff.[VarianceCi95Upper],
eff.[VarianceCi95Lower],
eff.[VarianceCi95Value],
eff.[VarianceCi90Upper],
eff.[VarianceCi90Lower],
eff.[VarianceCi90Value],
eff.[VarianceRangeUpper],
eff.[VarianceRangeLower],
eff.[VarianceIqrUpper],
eff.[VarianceIqrLower],
eff.[VarianceIqrValue],
eff.EventCriteriaScore as Measurement_EventCriteriaScore,
eff.[EventCriteriaRangeUpper] as Measurement_EventCriteriaScoreUpper,
eff.[EventCriteriaRangeLower] as Measurement_EventCriteriaScoreLower,
sta.StudyArea,
aemode.Name as Measurement_Mode,
stat.Name as [State],
bioname.Name AS Measurement_Name,
evcriop.Name as Measurement_EventCriteriaOperator,
evcrinum.Name as Measurement_EventCriteriaUnitNumerator,
evcrinum.Name as Measurement_EventCriteriaUnitDenominator,
valtype.Name AS Measurement_ValueType,
vartype.Name AS Measurement_VarianceType,
biosource.Name AS Measurement_Source,
effunitnum.Name AS Measurement_UnitNumerator,
effunitden.Name as Measurement_UnitDenominator,
efftype.Name AS Measurement_Type,
tpunit.Name AS Time_Unit


from dbo.Study st
left join dbo.studyassignment sta on sta.studyid=st.id
left join dbo.studyassignmentstate stat on stat.Id=sta.StudyAssignmentStateid
left join dbo.StudyUser stu on stu.Id=sta.StudyUserId
left join dbo.observation ob on ob.StudyAssignmentId=sta.id
left join dbo.observationtreatmentgroup obtg on obtg.observationid=ob.id
left join dbo.Timepoint tp on tp.observationtreatmentgroupid=obtg.Id

-- Treatments
left join dbo.observationtgtreatment obtx on obtx.observationtreatmentgroupid=obtg.id
left join dbo.treatment tx on tx.timepointid=tp.id
left join dbo.treatmenttype txname on txname.id=tx.treatmenttypeid
left join dbo.QualifierType txq on tx.[QualifierTypeId]=txq.Id
left join dbo.Measurementunittype tpunit on tpunit.Id = tp.DurationMeasurementUnitTypeId

-- Outcomes
left join dbo.Efficacy eff on eff.TimepointId = tp.Id
left join dbo.BiomarkerType bioname on bioname.Id=eff.BiomarkerTypeId
left join dbo.EfficacyType efftype on efftype.Id=eff.EfficacyTypeId
left join dbo.BiomarkerSourceType biosource on biosource.Id=eff.BiomarkerSourceTypeId
left join dbo.MeasurementUnitType effunitnum on effunitnum.Id=eff.[BiomarkerMeasurementUnitTypeId]
left join dbo.MeasurementUnitType effunitden on effunitden.Id=eff.[BiomarkerDenominatorMeasurementUnitTypeId]
left join dbo.VarianceType vartype on vartype.Id=eff.VarianceTypeId
left join dbo.EfficacyValueType valtype on valtype.Id=eff.EfficacyValueTypeId
left join dbo.EqualityOperatorType evcriop on evcriop.Id = eff.[EqualityOperatorTypeId]
left join dbo.MeasurementUnitType evcrinum on evcrinum.Id=eff.[EventCriteriaMeasurementUnitTypeId]
left join dbo.MeasurementUnitType evcriden on evcriden.Id=eff.[EventCriteriaDenominatorMeasurementUnitTypeId]
left join dbo.adverseeventmodetype aemode on aemode.Id=eff.AdverseEventModeTypeId

where sta.StudyArea like '%coronavirus%' and efftype.Name in ('Biomarker', 'Other Measurement') and stat.Name = 'Finalized'),

ae as (
select distinct
st.PubMedId as SourceId, 
st.MedAwareId,
ob.Id as ObservationId,
obtg.Id as ObservationTreatmentGroupId,
tp.IsBaseline as Time_IsBaseline,
tp.Duration as Time_Interval,

--Outcomes
eff.Mean as Measurement_Mean, 
eff.Median as Measurement_Median,
eff.ChangeInMean as Measurement_ChangeInMean,
eff.ChangeInMedian as Measurement_ChangeInMedian,
eff.NumberOfPatients as Measurement_Number,
eff.PercentOfPatients as Measurement_Percent,
eff.NumberOfEvents AS Measurement_nEvents,
eff.PercentOfEvents AS Measurement_percEvents,
eff.VarianceSd as Measurement_SD, 
eff.VarianceSe AS Measurement_SE,
eff.[VarianceCi99Upper],
eff.[VarianceCi99Lower],
eff.[VarianceCi99Value],
eff.[VarianceCi95Upper],
eff.[VarianceCi95Lower],
eff.[VarianceCi95Value],
eff.[VarianceCi90Upper],
eff.[VarianceCi90Lower],
eff.[VarianceCi90Value],
eff.[VarianceRangeUpper],
eff.[VarianceRangeLower],
eff.[VarianceIqrUpper],
eff.[VarianceIqrLower],
eff.[VarianceIqrValue],
eff.EventCriteriaScore as Measurement_EventCriteriaScore,
eff.[EventCriteriaRangeUpper] as Measurement_EventCriteriaScoreUpper,
eff.[EventCriteriaRangeLower] as Measurement_EventCriteriaScoreLower,
sta.StudyArea,
aemode.Name as Measurement_Mode,
stat.Name as [State],
aename.Name AS Measurement_Name,
evcriop.Name as Measurement_EventCriteriaOperator,
evcrinum.Name as Measurement_EventCriteriaUnitNumerator,
evcrinum.Name as Measurement_EventCriteriaUnitDenominator,
valtype.Name AS Measurement_ValueType,
vartype.Name AS Measurement_VarianceType,
biosource.Name AS Measurement_Source,
effunitnum.Name AS Measurement_UnitNumerator,
effunitden.Name as Measurement_UnitDenominator,
efftype.Name AS Measurement_Type,
tpunit.Name AS Time_Unit


from dbo.Study st
left join dbo.studyassignment sta on sta.studyid=st.id
left join dbo.studyassignmentstate stat on stat.Id=sta.StudyAssignmentStateid
left join dbo.StudyUser stu on stu.Id=sta.StudyUserId
left join dbo.observation ob on ob.StudyAssignmentId=sta.id
left join dbo.observationtreatmentgroup obtg on obtg.observationid=ob.id
left join dbo.Timepoint tp on tp.observationtreatmentgroupid=obtg.Id

-- Treatments
left join dbo.observationtgtreatment obtx on obtx.observationtreatmentgroupid=obtg.id
left join dbo.treatment tx on tx.timepointid=tp.id
left join dbo.treatmenttype txname on txname.id=tx.treatmenttypeid
left join dbo.QualifierType txq on tx.[QualifierTypeId]=txq.Id
left join dbo.Measurementunittype tpunit on tpunit.Id = tp.DurationMeasurementUnitTypeId

-- Outcomes
left join dbo.Efficacy eff on eff.TimepointId = tp.Id
left join dbo.Condition aename on aename.Id=eff.ConditionId
left join dbo.EfficacyType efftype on efftype.Id=eff.EfficacyTypeId
left join dbo.BiomarkerSourceType biosource on biosource.Id=eff.BiomarkerSourceTypeId
left join dbo.MeasurementUnitType effunitnum on effunitnum.Id=eff.[BiomarkerMeasurementUnitTypeId]
left join dbo.MeasurementUnitType effunitden on effunitden.Id=eff.[BiomarkerDenominatorMeasurementUnitTypeId]
left join dbo.VarianceType vartype on vartype.Id=eff.VarianceTypeId
left join dbo.EfficacyValueType valtype on valtype.Id=eff.EfficacyValueTypeId
left join dbo.EqualityOperatorType evcriop on evcriop.Id = eff.[EqualityOperatorTypeId]
left join dbo.MeasurementUnitType evcrinum on evcrinum.Id=eff.[EventCriteriaMeasurementUnitTypeId]
left join dbo.MeasurementUnitType evcriden on evcriden.Id=eff.[EventCriteriaDenominatorMeasurementUnitTypeId]
left join dbo.adverseeventmodetype aemode on aemode.Id=eff.AdverseEventModeTypeId

where sta.StudyArea like '%coronavirus%' and efftype.Name = 'Adverse Event' and stat.Name = 'Finalized'),
eff_total as (
select * from om
union all 
select * from biomarker
union all
select * from ae
)
select * from eff_total e
