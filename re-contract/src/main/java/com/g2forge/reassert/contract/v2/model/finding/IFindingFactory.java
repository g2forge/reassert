package com.g2forge.reassert.contract.v2.model.finding;

import com.g2forge.alexandria.analysis.ISerializableFunction1;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.express.v2.model.IExplained;

@FunctionalInterface
public interface IFindingFactory<Finding extends IFinding> extends ISerializableFunction1<IExplained<TermRelation>, Finding> {
	public default ITypeRef<?> getFindingType() {
		return asMethodAnalyzer().getReturnType();
	}
}
