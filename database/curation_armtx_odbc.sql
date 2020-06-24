select distinct
	st.PubMedId as SourceId, 
	st.MedAwareId,
	ob.Id as ObservationId,
	obtg.Id as ObservationTreatmentGroupId,
	stat.Name as State,
	dt.Name AS Study_DesignTemplate,
	loc.Name AS Study_Location,
	ob.YearOfPublication as Study_PublicationDate,
	ob.StudyDurationWeeks as Study_DurationWeeks,
	ob.IsRandomized as Study_IsRandomized,
	ob.IsPlaceboIncluded as Study_IsPlaceboControlled, 
	ob.IsStudyControlled as Study_IsControlled,
	obc.NumberOfPatients as Condition_NumberOfPatients,
	obc.PercentOfPatients as Condition_PercentOfPatients,
	obtg.Id as ArmGroupId,
	obtg.Name AS Arm_Name,
	obtg.IsControl as Arm_IsControl,
	obtg.IsTotalGroup as Arm_IsTotal,
	obtg.AgeEnrollmentMean as Arm_MeanAge,
	obtg.AgeEnrollmentLow as Arm_AgeEnrollmentLow,
	obtg.AgeEnrollmentHigh as Arm_AgeEnrollmentHigh,
	obtg.GenderPercentFemale AS Arm_PercentFemale,
	obtg.GenderPercentMale AS Arm_PercentMale,
	obtg.NumberOfParticipants as Arm_Participants,
	tp.IsBaseline as Time_IsBaseline,
	tp.Duration as Time_Interval,
	txname.Name as Treatment_Name,
	tx.Numberofpatients AS Treatment_NumberOfParticipants,
	tx.PercentOfPatients as Treatment_PercentOfParticipants,
	sta.StudyArea,
	txmode.Name AS Treatment_Mode,
	sut.Name AS Treatment_SurgeryType,
	suloc.Name as Treatment_SurgeryLocation,
	q.name as Qualifier,
	unit.Name AS Time_Unit,
	cond.Name AS Condition,
	ob.AuthorLastName as Study_Author

                 
                 
                 
from dbo.Study st
left join dbo.studyassignment sta on sta.studyid=st.id
left join dbo.studyassignmentstate stat on stat.Id=sta.StudyAssignmentStateid
left join dbo.StudyUser stu on stu.Id=sta.StudyUserId
left join dbo.StudyAssignmentType utype on utype.Id=sta.StudyAssignmentTypeId
left join dbo.DesignTemplate dt on dt.Id=sta.DesignTemplateId
left join dbo.observation ob on ob.StudyAssignmentId=sta.id
left join dbo.CountryType loc on loc.Id=ob.CountryTypeId
left join dbo.ObservationCondition obc on obc.ObservationId=ob.Id
left join dbo.Condition cond on cond.Id=obc.ConditionId
left join dbo.observationtreatmentgroup obtg on obtg.observationid=ob.id
left join dbo.Timepoint tp on tp.observationtreatmentgroupid=obtg.Id
left join dbo.observationtgtreatment obtx on obtx.observationtreatmentgroupid=obtg.id
left join dbo.treatment tx on tx.timepointid=tp.id
left join dbo.treatmenttype txname on txname.id=tx.treatmenttypeid
left join dbo.QualifierType q on tx.[QualifierTypeId]=q.Id
left join dbo.Measurementunittype unit on unit.Id = tp.DurationMeasurementUnitTypeId
left join dbo.treatmentmodetype txmode on txmode.Id=tx.TreatmentModeTypeId
left join dbo.SurgeryType sut on sut.Id=tx.SurgeryTypeId
left join dbo.SurgeryLocationType suloc on suloc.Id=tx.SurgeryLocationTypeId
                 
where sta.StudyArea like '%coronavirus%'  and
	stat.Name = 'Finalized'

