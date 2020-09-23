package com.g2forge.reassert.core.model.contract;

import java.util.stream.Stream;

import com.g2forge.alexandria.java.core.helpers.HStream;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.terms.ITerm;
import com.g2forge.reassert.core.model.contract.usage.IUsage;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum ContractType {
	License(ITypeRef.of(ILicenseTerm.class), ITypeRef.of(ILicense.class)),
	Usage(ITypeRef.of(IUsageTerm.class), ITypeRef.of(IUsage.class));

	public static ContractType valueOf(IContract contract) {
		return HStream.findOne(Stream.of(ContractType.values()).filter(type -> type.getContractType().isInstance(contract)));
	}
	
	public static ContractType valueOf(ITerm term) {
		return HStream.findOne(Stream.of(ContractType.values()).filter(type -> type.getTermType().isInstance(term)));
	}

	protected final ITypeRef<? extends ITerm> termType;
	
	protected final ITypeRef<? extends IContract> contractType;
}