package com.g2forge.reassert.contract.term;

import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

/**
 * Terms adapted from <a href="https://choosealicense.com/appendix/">choose-a-license</a>.
 */
@ReassertLegalOpinion
@Getter
@RequiredArgsConstructor
public enum StandardLicenseTerm implements ILicenseTerm {
	CommercialUse(Type.Permission, "Permission for commercial usage"),
	Distribution(Type.Permission, "Permission to distribute"),
	Modification(Type.Permission, "Permission to modify"),
	PatentGrant(Type.Permission, "Permission to use patents"),
	PrivateUse(Type.Permission, "Permission for private usage"),
	DisclosureSource(Type.Condition, "Requires source disclosure"),
	Notice(Type.Condition, "Requires copyright notice"),
	SaaSIsDistribution(Type.Condition, "Requires distribution for SaaS"),
	SameLicense(Type.Condition, "Requires compatible licenses"),
	StateChanges(Type.Condition, "Requires statement of changes"),
	PatentNonGrant(Type.Limitation, "Does not grant patent usage"),
	Liability(Type.Limitation, "Does not idemnify against liability"),
	Trademark(Type.Limitation, "Does not grant trademark usage"),
	Warranty(Type.Limitation, "Does not grant warranty");

	protected final Type type;

	protected final String description;

	@Override
	public String getName() {
		return name();
	}
}
